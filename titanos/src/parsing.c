#include "parsing.h"
#include "lexer.h"
#include "parser.h"
#include <stdio.h>
#include "string_utils.h"
#include "symbol.c"
#include "ast_types.h"
#include "attributes.h"
#include "error.h"
#include "module.h"
#include "arena_allocator.h"
#include "diagnostics.h"
#include "file.h"
#include "common.h"
#include <stdarg.h>
#include "types/type.h"
#include "decl.h"
#include "expr.h"
#include "builtins.h"

Parser *current_parser;
Token tok;
Token prev_tok;

#define CONSUME_EOS_OR_EXIT if (!consume(TOKEN_EOS, "Expected ';'")) return NULL
#define CONSUME_START_BRACE_OR_EXIT if (!consume(TOKEN_LBRACE, "Expected '{'")) return NULL
#define CONSUME_END_BRACE_OR_EXIT if (!consume(TOKEN_RBRACE, "Expected '}'")) return NULL
#define UPDATE(__X) do { token_expand(&__X->span, &prev_tok); } while (0)
#define UPDATE_AND_RETURN(__X) do { UPDATE(__X); return __X; } while (0)
#define CONSUME_REQ_EOS_AND_RETURN(__X) do { UPDATE(__X); CONSUME_EOS_OR_EXIT; return __X; } while(0)

static inline Decl *new_var_decl(Token *start, Token *name, VarDeclKind kind, Type *type, bool public)
{
	Decl *declaration = decl_new(DECL_VAR, start, name, false);
	declaration->var.type = type;
	declaration->var.kind = kind;
	declaration->var.in_init = false;
	return declaration;
}
static inline bool has_attributes()
{
    return tok.type == TOKEN_AT;
}

static Ast *stmt_to_compound(Ast *inner_ast)
{
	if (!inner_ast || inner_ast->ast_id == AST_COMPOUND_STMT) return inner_ast;
	Ast *ast = new_ast_with_span(AST_COMPOUND_STMT, &inner_ast->span);
	Vector *stmts = new_vector(1);
	vector_add(stmts, inner_ast);
	ast->compound_stmt.stmts = stmts;
	return ast;
}

static inline bool current_is(token_type type)
{
	return tok.type == type;
}


typedef Expr *(*ParseFn)(Expr *);

typedef enum _Precedence
{
	PREC_NONE,
	PREC_COMMA,
	PREC_ASSIGNMENT,  // =, *=, /=, %=, ...
	PREC_TERNARY,     // ?:
	PREC_OR,          // && ||
	PREC_COMPARISON,  // < > <= >= == !=
	PREC_TERM,        // + -
	PREC_BITWISE,     // ^ | &
	PREC_SHIFT,       // << >> >>>
	PREC_FACTOR,      // * / %
	PREC_POW,         // **
	PREC_UNARY,       // ! - + ~ * & prefix ++ prefix --
	PREC_CALL,        // . () [] postfix ++ postfix --
} Precedence;

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;


static ParseRule rules[TOKEN_EOF + 1];


static Expr *parse_expression(void);
static Ast *parse_stmt(void);
static Expr *parse_init_value(void);


void error_at_current(const char *message)
{
	error_at(&tok, message);
}


static void error(const char *message)
{
	error_at(&prev_tok, message);
}


static void advance(void)
{
	prev_tok = tok;

	while (1)
	{
		tok = scan_token();
		// printf(">>> %.*s => %s\n", tok.length, tok.start, token_type_to_string(tok.type));
		if (tok.type != TOKEN_ERROR) break;
	}
}

static inline void advance_and_verify(token_type type)
{
	assert(current_is(type) && "Not case");
	advance();
}

static inline bool try_consume(token_type type)
{
	if (current_is(type))
	{
		advance();
		return true;
	}
	return false;
}

static bool consume(token_type type, const char *message, ...)
{
	if (try_consume(type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	verror_at(&tok, message, args);
	va_end(args);
	return false;
}


void recover_to(token_type type)
{
	if (!in_panic_mode()) return;
	while (!current_is(type))
	{
		advance();
		if (current_is(TOKEN_EOF)) return;
	}
	advance();
	reset_panic_mode();
}


/**
 * import ::= IMPORT IDENTIFIER EOS
 *          | IMPORT IDENTIFIER AS IDENTIFIER EOS
 *          | IMPORT IDENTIFIER LOCAL EOS
 */
static Decl *parse_import()
{
	advance_and_verify(TOKEN_IMPORT);

	Decl *decl = decl_new(DECL_IMPORT, &prev_tok, &tok, false);
	if (!consume(TOKEN_IDENTIFIER, "Expected an identifier"))
	{
		recover_to(TOKEN_EOS);
		return NULL;
	}

	if (try_consume(TOKEN_AS))
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected alias name"))
		{
			return NULL;
		}
		if (token_compare(&prev_tok, &current_parser->current_module))
		{
			error_at(&prev_tok, "Cannot set alias to the current module");
			return NULL;
		}
		if (!is_lower(&prev_tok))
		{
			error_at(&prev_tok, "Alias must start with a lower case letter");
			return NULL;
		}
		decl->import.type = IMPORT_TYPE_ALIAS;
		decl->import.alias = prev_tok;
	}
	else if (try_consume(TOKEN_LOCAL))
	{
		decl->import.type = IMPORT_TYPE_LOCAL;
	}
	CONSUME_EOS_OR_EXIT;

	if (token_compare(&decl->name, &current_parser->current_module))
   	{
   		error_at(&prev_tok, "Cannot import the current module");
        return NULL;
   	}
	UPDATE_AND_RETURN(decl);
}



/**
 * module ::= MODULE IDENTIFIER EOS
 */
Decl *parse_module()
{
	// No module? Error?
	if (!try_consume(TOKEN_MODULE))
	{
		error_at_current("Expected 'module' at the start of file");
		return NULL;
	}
	Decl *import = decl_new(DECL_IMPORT, &prev_tok, &tok, false);
	import->is_used = true;
	import->import.type = IMPORT_TYPE_LOCAL;
	if (!consume(TOKEN_IDENTIFIER, "Expected module name")) return NULL;
	if (prev_tok.length > 1 && prev_tok.start[0] == '_' && prev_tok.start[1] == '_')
	{
		error_at(&prev_tok, "Invalid module name");
	}
	if (!is_lower(&prev_tok))
	{
		error_at(&prev_tok, "Module must start with lower case");
	}
	if (token_compare_str(&prev_tok, "c2"))
	{
		error_at(&prev_tok, "'c2' module name is reserved");
	}
	if (token_compare_str(&prev_tok, "main"))
	{
		error_at(&prev_tok, "'main' module name is reserved");
	}
	current_parser->current_module = prev_tok;
	CONSUME_EOS_OR_EXIT;
	vector_add(current_parser->imports, import);
	UPDATE_AND_RETURN(import);
}


static Expr *parse_string_literal(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_string = expr_new(EXPR_CONST, &prev_tok);

	const char *str = prev_tok.start;
	uint32_t len = prev_tok.length;

	// Just keep chaining if there are multiple parts.

	while (try_consume(TOKEN_STRING))
	{
		char *new_string = malloc_arena(len + prev_tok.length);
		memcpy(new_string, str, len);
		memcpy(new_string + len, prev_tok.start, prev_tok.length);
		str = new_string;
		len += prev_tok.length;
	}
	expr_string->const_expr.value = value_new_string(str, len);
	expr_string->const_state = CONST_FULL;
	expr_string->type = type_string();
	UPDATE_AND_RETURN(expr_string);
}

static Expr *parse_integer(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = expr_new(EXPR_CONST, &prev_tok);
	number->const_expr.value = parse_int(prev_tok.start, prev_tok.length);
	number->const_state = CONST_FULL;
	number->type = type_compint();
	return number;
}


static Expr *parse_double(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = expr_new(EXPR_CONST, &prev_tok);
	number->const_state = CONST_FULL;
	char *end = NULL;
	long double fval = strtold(prev_tok.start, &end);
	if (end != prev_tok.length + prev_tok.start)
	{
		error_at(&prev_tok, "Invalid float");
		return NULL;
	}
	number->const_expr.value = value_new_float(fval);
	number->type = type_compfloat();
	return number;
}

static Expr *parse_bool(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = expr_new(EXPR_CONST, &prev_tok);
	number->const_state = CONST_FULL;
	number->const_expr.value = value_new_bool(tok.type == TOKEN_TRUE);
	number->type = type_builtin_bool();
	return number;
}

static Expr *parse_nil(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *value = expr_new(EXPR_CONST, &prev_tok);
	value->const_expr.value = value_nil();
	value->const_state = CONST_FULL;
	value->type = type_nil();
	return value;
}


/**
 * attributes ::= '@' (' attribute_list ')'
 *
 * attribute_list ::= attribute
 *                    attribute_list ',' attribute
 *
 * attribute ::= name
 *             | name '=' STRING
 */
Vector *parse_attributes()
{
	advance_and_verify(TOKEN_AT);

	if (!consume(TOKEN_LPAREN, "Missing '(' after '@'")) return NULL;

	Vector *list = new_vector(4);

	while (true)
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
        Ast *attribute = new_ast_with_span(AST_ATTRIBUTE, &prev_tok);
        attribute->attribute.name = prev_tok;
        Expr *arg = NULL;
		if (try_consume(TOKEN_EQ))
		{
			arg = parse_expression();
		}
		attribute->attribute.value = arg;
        vector_add(list, attribute);
		if (!try_consume(TOKEN_COMMA)) break;
	}
	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;
	return list;
}

static bool optional_add_attributes(Vector **entry)
{
    if (tok.type != TOKEN_AT)
    {
        *entry = NULL;
        return true;
    }
    Vector *attributes = parse_attributes();
    if (!attributes) return false;
    *entry = attributes;
    return true;
}

static inline ParseRule *get_rule(token_type type)
{
	return &rules[type];
}


/**
 * identifier ::= IDENTIFIER
 * @param left (must be null)
 * @return The identifier node
 */
static Expr *parse_identifier(Expr *left)
{
	assert(!left && "Unexpected left hand side");

	Expr *expr = expr_new(EXPR_IDENTIFIER, &prev_tok);
	expr->identifier_expr.identifier = prev_tok;
	return expr;
}

/**
 * full_identifier ::= IDENTIFIER
 *                     IDENTIFIER '.' IDENTIFIER
 * @return the resulting identifier or NULL if an error occurs.
 */
Expr *parse_full_identifier()
{
	if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
	Expr *expr = expr_new(EXPR_IDENTIFIER, &prev_tok);

	expr->identifier_expr.identifier = prev_tok;

	if (try_consume(TOKEN_DOT))
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected identifier after ','")) return NULL;
		Expr *sub = expr_new(EXPR_IDENTIFIER, &prev_tok);
		sub->identifier_expr.identifier = prev_tok;
		Expr *result = expr_new(EXPR_ACCESS, &expr->span);
		result->access_expr.parent = expr;
		result->access_expr.sub_element = sub;
		UPDATE_AND_RETURN(result);
	}
	UPDATE_AND_RETURN(expr);
}



static Expr *parse_precedence_after_advance(Precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = get_rule(prev_tok.type)->prefix;
	if (prefix_rule == NULL)
	{
		error("Expected expression.");
		return NULL;
	}

	Expr *left_side = prefix_rule(NULL);
	while (precedence <= get_rule(tok.type)->precedence)
	{
		advance();
		ParseFn infix_rule = get_rule(prev_tok.type)->infix;
		left_side = infix_rule(left_side);
	}
	return left_side;
}

typedef enum _TypeQualifierd
{
	TYPE_QUALIFIER_NONE = 0,
	TYPE_QUALIFIER_CONST = 0x01,
	TYPE_QUALIFIER_VOLATILE = 0x02,
	TYPE_QUALIFIER_ALIAS = 0x04,
} TypeQualifier;

TypeQualifier read_optional_type_specifiers()
{
	TypeQualifier specifier = TYPE_QUALIFIER_NONE;
	while (1)
	{
		if (try_consume(TOKEN_CONST))
		{
			specifier |= TYPE_QUALIFIER_CONST;
		}
		else if (try_consume(TOKEN_VOLATILE))
		{
			specifier |= TYPE_QUALIFIER_VOLATILE;
		}
		else if (try_consume(TOKEN_ALIAS))
		{
			specifier |= TYPE_QUALIFIER_ALIAS;
		}
		else
		{
			return specifier;
		}
	}
}

static void add_type_specifier(Type *type, TypeQualifier type_specifier)
{
	if (!type_specifier) return;
	if (type_specifier & TYPE_QUALIFIER_CONST)
	{
		type->is_const = true;
	}
	if (type_specifier & TYPE_QUALIFIER_VOLATILE)
	{
		type->is_volatile = true;
	}
	if (type_specifier & TYPE_QUALIFIER_ALIAS)
	{
		type->is_aliased = true;
	}
}

static Type *create_pointer_type(Type *base)
{
	Type *pointer = new_type(TYPE_POINTER, base->is_public);
	pointer->pointer.base = base;
	return pointer;
}

/**
 * array_type ::= '[' expression ']'
 *              | '[' ']'
 * @param base the base type
 * @return the resulting type or NULL if parsing failed.
 */
Type *parse_array_type(Type *base)
{
	assert(base && "Cannot have null expression");
	advance_and_verify(TOKEN_LBRACKET);
	Type *type = new_type(TYPE_ARRAY, base->is_public);
	type->array.base = base;
	if (try_consume(TOKEN_RBRACKET))
	{
		type->array.is_empty = true;
		return type;
	}
	Expr *expression = parse_expression();
	if (!expression) return NULL;
	if (!consume(TOKEN_RBRACKET, "Missing ']'")) return NULL;
	type->array.len_expr = expression;
	return type;
}

/**
 * type_specifier ::= full_identifier const?
 *                  | type_qualifier array_specifier const?
 *                  | type_qualifier '*' const?
 *                  |
 * @param accept_qualifiers read optional type specifiers (volatile, const etc)
 * @param public is this public
 * @return type
 */
Type *parse_type(bool accept_qualifiers, bool public)
{
	// IMPROVE AST span.
	TypeQualifier type_qualifier = accept_qualifiers ? read_optional_type_specifiers() : TYPE_QUALIFIER_NONE;
	Type *base;
	if (try_consume(TOKEN_VOID))
	{
		base = void_type();
		if (type_qualifier)
		{
			error_at(&prev_tok, "Can't add type specifiers to void");
			return NULL;
		}
	}
	else
	{
		Expr *expr = parse_full_identifier();
		if (!expr) return NULL;
		base = new_unresolved_type(expr, public);
		add_type_specifier(base, type_qualifier);
	}
	while (true)
	{
		Token first_specifier = tok;
		type_qualifier = read_optional_type_specifiers();
		switch (tok.type)
		{
			case TOKEN_LBRACKET:
				base = parse_array_type(base);
				break;
			case TOKEN_STAR:
				advance();
				base = create_pointer_type(base);
				break;
			default:
				if (type_qualifier)
				{
					error_at(&first_specifier, "Unexpected type specifier here");
				}
				return base;
		}
		add_type_specifier(base, type_qualifier);
	}
}

static Expr *parse_expression(void)
{
	advance();
	return parse_precedence_after_advance(PREC_ASSIGNMENT);
}

static Expr *parse_precedence(Precedence precedence)
{
	advance();
	return parse_precedence_after_advance(precedence);
}



/**
 * param_decl ::= param_decl
 *              | param_decl IDENT
 *              | param_decl IDENT '=' expr
 */
static inline Decl *parse_param_decl(bool public, bool has_default_args)
{
	Token start = tok;
	Type *type = parse_type(true, public);
	if (!type) return NULL;

    Decl *param_decl = new_var_decl(&start, &start, VARDECL_PARAM, type, public);
	if (!try_consume(TOKEN_IDENTIFIER))
	{
		param_decl->name.length = 0;
	}
	else
	{
		param_decl->name = prev_tok;
	}

	if (try_consume(TOKEN_EQEQ))
	{
		if (!param_decl->name.length)
		{
			error_at(&prev_tok, "Expected variable name with default value");
			return NULL;
		}
		param_decl->var.init_expr = parse_expression();
		if (!param_decl->var.init_expr) return NULL;
	}

	if (has_default_args && !param_decl->var.init_expr)
	{
		error_at(&param_decl->span, "Parameters without default value must preceed the ones with default");
	}
	UPDATE_AND_RETURN(param_decl);
}

/**
 * param_list ::= '...'
 *                | param_decl
 *                | param_list ',' param_list
 *                ;
 * @return ast or NULL
 */
static inline bool parse_argument_list(FuncDecl *func_decl, bool public, bool type_only)
{
	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	if (try_consume(TOKEN_RPAREN))
	{
	    func_decl->args = new_vector(0);
	    func_decl->variadic = false;
	    return true;
	}
	Vector *args = new_vector(6);
	func_decl->variadic = false;
	bool had_defaults = false;
	while (true)
	{
		if (try_consume(TOKEN_ELIPSIS))
		{
            func_decl->variadic = true;
		}
		else
		{
			Decl *decl = parse_param_decl(public, had_defaults);
			if (!decl) return NULL;
			if (decl->var.init_expr)
			{
				had_defaults = true;
				if (type_only)
				{
					error_at(&decl->var.init_expr->span, "Default arguments are not allowed here");
					had_defaults = false;
				}
			}
			vector_add(args, decl);
		}
		if (!try_consume(TOKEN_COMMA)) break;
		if (func_decl->variadic)
		{
			error_at_current("Variadic parameter followed by arguments");
			return NULL;
		}
	}
	func_decl->args = args;
	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;
	return true;
}

static inline Decl *parse_func_decl(bool public, bool type_only)
{
	Token start = tok;
	advance_and_verify(TOKEN_FUNC);

    Type *return_type = parse_type(true, public);
	if (!return_type) return NULL;

	if (!consume(TOKEN_IDENTIFIER, "Expected function name but got '%.*s'", SPLAT_TOK(tok))) return NULL;

    Decl *decl = decl_new(DECL_FUNC, &start, &prev_tok, public);
	decl->func_decl.rtype = return_type;

	if (try_consume(TOKEN_DOT))
	{
		if (consume(TOKEN_IDENTIFIER, "Expected struct function name but got '%.*s'", SPLAT_TOK(tok))) return NULL;

		char *full_name = malloc_arena(prev_tok.length + decl->name.length + 2);
		snprintf(full_name, decl->name.length + prev_tok.length + 1,
				"%.*s.%.*s", SPLAT_TOK(decl->name), SPLAT_TOK(prev_tok));
		decl->func_decl.full_name = token_wrap(full_name);
		decl->name = prev_tok;
		decl->func_decl.is_struct_func = true;
	}

    if (!parse_argument_list(&decl->func_decl, public, type_only)) return NULL;

	if (!optional_add_attributes(&decl->attributes)) return NULL;


	UPDATE_AND_RETURN(decl);
}

static Ast *parse_compound_stmt()
{
    CONSUME_START_BRACE_OR_EXIT;
	Ast *compound = new_ast_with_span(AST_COMPOUND_STMT, &prev_tok);
	Vector *stmts = new_vector(0);
	compound->compound_stmt.stmts = stmts;
	while (1)
	{
		if (try_consume(TOKEN_RBRACE)) return end_ast(compound, &prev_tok);
		Ast *stmt = parse_stmt();
		if (!stmt)
		{
			recover_to(TOKEN_RBRACE);
			return NULL;
		}
		vector_add(stmts, stmt);
	}
}

/**
 * if_stmt ::= if_start stmt
 *             if_start compound_stmt ELSE compound_stmt
 *
 * if_start ::= IF '(' expression ')'
 *
 * @return Ast* is parsing succeeded NULL otherwise
 */
static Ast *parse_if_stmt()
{
	// Advance past if
	advance_and_verify(TOKEN_IF);
	Ast *if_ast = new_ast_with_span(AST_IF_STMT, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Expected (")) return NULL;

	Expr *expr = parse_expression();

	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;

	// Prefer it in compound form.
	Ast *if_body = parse_stmt();
	if (!if_body) return NULL;
	if (if_body->ast_id != AST_COMPOUND_STMT && current_is(TOKEN_ELSE))
	{
		error_at_current("if-else requires the format if {  } else {  }");
		return NULL;
	}
	Ast *else_body = NULL;
	if (try_consume(TOKEN_ELSE))
	{
		else_body = parse_compound_stmt();
		if (!else_body) return NULL;
	}
	if_ast->if_stmt.else_body = stmt_to_compound(else_body);
	if_ast->if_stmt.then_body = stmt_to_compound(if_body);
	if_ast->if_stmt.expr = expr;
	UPDATE_AND_RETURN(if_ast);
}

/**
 * while_stmt ::= WHILE '(' expression ')' statement
 * @return Ast* is parsing succeeded, NULL otherwise.
 */
static Ast *parse_while_stmt()
{
	// Advance past while
	advance_and_verify(TOKEN_WHILE);
	Ast *while_ast = new_ast_with_span(AST_WHILE_STMT, &prev_tok);
	if (!consume(TOKEN_LPAREN, "Expected (")) return NULL;
	Expr *expr = parse_expression();
	if (!expr) return NULL;
	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	Ast *body = parse_stmt();
	if (!body) return NULL;
	while_ast->while_stmt.body = stmt_to_compound(body);
	while_ast->while_stmt.expr = expr;
	UPDATE_AND_RETURN(while_ast);
}

/**
 * do_stmt ::= DO compound_stmt WHILE '(' expression ')' EOS
 *
 * @return Ast* if parsing succeeds, NULL otherwise
 */
static Ast *parse_do_stmt()
{
	// Advance past do
	advance_and_verify(TOKEN_DO);
	Ast *do_ast = new_ast_with_span(AST_DO_STMT, &prev_tok);

	// Require compound stmt!
	Ast *body = parse_compound_stmt();
	if (!body) return NULL;
	if (!consume(TOKEN_WHILE, "Expected 'while'")) return NULL;
	if (!consume(TOKEN_LPAREN, "Expected (")) return NULL;
	Expr *expr = parse_expression();
	if (!expr) return NULL;
	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	if (!consume(TOKEN_EOS, "Expected ';'")) return NULL;
	do_ast->ast_id = AST_DO_STMT;
	do_ast->do_stmt.body = body;
	do_ast->do_stmt.expr = expr;
	UPDATE_AND_RETURN(do_ast);
}

/**
 * defer_stmt ::= DEFER stmt
 * @return Ast* if parsing succeeds NULL otherwise
 */
static Ast *parse_defer_stmt()
{
	advance_and_verify(TOKEN_DEFER);
	Ast *defer_stmt = new_ast_with_span(AST_DEFAULT_STMT, &prev_tok);
	Ast *defer_body = parse_stmt();
	if (!defer_body) return NULL;
	defer_stmt->defer_stmt.body = defer_body;
	defer_stmt->defer_stmt.emit_boolean = false;
	defer_stmt->defer_stmt.prev_defer = NULL;
	UPDATE_AND_RETURN(defer_stmt);
}

/**
 * case_stmt ::= CASE expression ':'
 *               | case_stmt statement
 * @return Ast* or NULL if parsing succeeds.
 */
static Ast *parse_case_stmt()
{
	advance_and_verify(TOKEN_CASE);
	Ast *ast = new_ast_with_span(AST_CASE_STMT, &prev_tok);
	Expr *expression = parse_expression();
	if (!expression) return NULL;
	if (!consume(TOKEN_COLON, "Missing ':' after case")) return NULL;
	Vector *stmts = new_vector(4);
	while (!current_is(TOKEN_CASE) && !current_is(TOKEN_DEFAULT) && !current_is(TOKEN_RBRACE))
	{
		Ast *stmt = parse_stmt();
		if (!stmt) return NULL;
		vector_add(stmts, stmt);
	}
	ast->case_stmt.expr = expression;
	ast->case_stmt.stmts = stmts;
	UPDATE_AND_RETURN(ast);
}

/**
 * default_stmt ::= DEFAULT ':'
 *                  | default_stmt statement
 * @return Ast* or NULL if parsing failed.
 */
static Ast *parse_default_stmt()
{
	advance_and_verify(TOKEN_DEFAULT);
	Ast *ast = new_ast_with_span(AST_DEFAULT_STMT, &prev_tok);
	if (!consume(TOKEN_COLON, "Missing ':' after default")) return NULL;
	Vector *stmts = new_vector(4);
	while (!current_is(TOKEN_CASE) && !current_is(TOKEN_DEFAULT) && !current_is(TOKEN_RBRACE))
	{
		Ast *stmt = parse_stmt();
		if (!stmt) return NULL;
		vector_add(stmts, stmt);
	}
	ast->default_stmt.stmts = stmts;
	UPDATE_AND_RETURN(ast);
}

/**
 * switch_stmt ::= SWITCH '(' expression ')' '{' switch_body '}'
 *
 * single_switch_stmt ::= case_stmt
 *                      | default_stmt
 * switch_body ::= single_switch_stmt
 *               | switch_body single_switch_stmt
 *
 * @return Ast* or NULL if parsing fails
 */
static Ast *parse_switch_stmt()
{
	advance_and_verify(TOKEN_SWITCH);

	Ast *ast = new_ast_with_span(AST_SWITCH_STMT, &prev_tok);
	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	Expr *expr = parse_expression();
	if (!expr) return NULL;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;


	ast->switch_stmt.expr = expr;

	CONSUME_START_BRACE_OR_EXIT;

	ast->switch_stmt.default_stmt = NULL;
	Vector *cases = new_vector(8);
	Ast *case_stmt;
	while (1)
	{
		switch (tok.type)
		{
			case TOKEN_CASE:
				case_stmt = parse_case_stmt();
				if (case_stmt) vector_add(cases, case_stmt);
				break;
			case TOKEN_DEFAULT:
				case_stmt = parse_default_stmt();
				if (!case_stmt) continue;
				if (ast->switch_stmt.default_stmt)
				{
					error_at(&case_stmt->span, "Default defined twice");
					error_at(&ast->switch_stmt.default_stmt->span, "Previous definition");
					continue;
				}
				ast->switch_stmt.default_stmt = case_stmt;
				break;
			case TOKEN_RBRACE:
				goto done;
			default:
				error_at_current("Expected 'case' or 'default'");
				recover_to(TOKEN_RBRACE);
				return NULL;
		}
	}
	done:
	advance_and_verify(TOKEN_RBRACE);
	ast->switch_stmt.case_list = cases;
	UPDATE_AND_RETURN(ast);
}

/**
 * break_stmt ::= BREAK EOS
 * @return Ast * or NULL if there is an error.
 */
static Ast *parse_break_stmt()
{
	advance_and_verify(TOKEN_BREAK);
	Ast *ast = new_ast_with_span(AST_BREAK_STMT, &prev_tok);
	CONSUME_REQ_EOS_AND_RETURN(ast);
}

/**
 * return_stmt ::= RETURN expression EOS
 * @return Ast* if parsing succeeded, NULL otherwise
 */
static Ast *parse_return_stmt()
{
	advance_and_verify(TOKEN_RETURN);
	Ast *ast = new_ast_with_span(AST_RETURN_STMT, &prev_tok);
	if (try_consume(TOKEN_EOS))
	{
		ast->return_stmt.expr = NULL;
		UPDATE_AND_RETURN(ast);
	}
	Expr *expression = parse_expression();
	if (!expression) return NULL;
	ast->return_stmt.expr = expression;
	CONSUME_REQ_EOS_AND_RETURN(ast);
}

/**
 * goto_stmt ::= GOTO expression EOS
 * @return Ast* or NULL if parsing failed.
 */
static Ast *parse_goto_stmt()
{
	advance_and_verify(TOKEN_GOTO);
	Ast *ast = new_ast_with_span(AST_GOTO_STMT, &prev_tok);

	Expr *expression = parse_expression();
	if (!expression) return NULL;
	ast->goto_stmt.label = expression;

	CONSUME_REQ_EOS_AND_RETURN(ast);
}

/**
 * continue_stmt ::= CONTINUE EOS
 * @return Ast *
 */
static Ast *parse_continue_stmt()
{
	advance_and_verify(TOKEN_CONTINUE);
	Ast *ast = new_ast_with_span(AST_CONTINUE_STMT, &prev_tok);
	CONSUME_REQ_EOS_AND_RETURN(ast);
}

/* Syntax (GNU extended asm statement)
 * asm-statement:
 *      'asm' type-qualifier[opt] '(' asm-argument ')' ';'
 *
 * asm-argument:
 *      asm-string-literal
 *      asm-string-literal ':' asm-operands[opt]
 *      asm-string-literal ':' asm-operands[opt] ':' asm-operands[opt]
 *      asm-string-literal ':' asm-operands[opt] ':' asm-operands[opt]
 *              ':' asm-clobbers
 *
 * asm-clobbers:
 *      asm-string-literal
 *      asm-clobbers ',' asm-string-literal
 */
static Ast *parse_asm_stmt()
{
	assert(current_is(TOKEN_ASM) && "Expected continue");
	advance();

	error_at_current("TODO");
	return NULL;
}
/**
 * expr_stmt ::= expression EOS
 * @return Ast * or NULL if parsing failed.
 */
static Ast *parse_expr_stmt()
{
	Expr *expression = parse_expression();
	if (!expression) return NULL;
	Ast *stmt = new_ast_with_span(AST_EXPR_STMT, &expression->span);
	stmt->expr_stmt.expr = expression;
	CONSUME_REQ_EOS_AND_RETURN(stmt);
}


/**
 * field_designator ::= '.' IDENTIFIER '=' expression
 *
 * @return Ast * if successfull, NULL on error.
 */
static Expr *parse_field_designator()
{
	advance_and_verify(TOKEN_DOT);
	Expr *expr = expr_new(EXPR_DESIGNATED_INITIALIZER, &prev_tok);

	if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;

	if (!consume(TOKEN_EQ, "Expected '=' after identifier")) return NULL;

	Expr *init_value = parse_init_value();
	if (!init_value) return NULL;

	expr->designated_initializer_expr.expr = init_value;
	UPDATE_AND_RETURN(expr);
}

/**
 * init_values ::= expression
 *               | field_designator
 *               | init_values ',' expression
 *               | init_values ',' field_designator
 *
 * @return Expr* or NULL if parsing fails.
 */
static Expr *parse_init_values()
{
	advance_and_verify(TOKEN_LBRACE);

	Expr *expr = expr_new(EXPR_STRUCT_INIT_VALUES, &tok);
	if (try_consume(TOKEN_RBRACE))
	{
		expr->struct_init_values_expr.values = new_vector(0);
		UPDATE_AND_RETURN(expr);
	}
	Vector *values = new_vector(4);
	expr->struct_init_values_expr.values = values;
	do
	{
		Expr *value = current_is(TOKEN_DOT) ? parse_field_designator() : parse_expression();
		if (!value) return NULL;
		vector_add(values, value);

	} while (try_consume(TOKEN_COMMA));
	CONSUME_END_BRACE_OR_EXIT;
	UPDATE_AND_RETURN(expr);
}

/**
 * init_value ::= expression
 *              | init_values
 * @return Expr* or NULL if an error occurs.
 */
static Expr *parse_init_value(void)
{
	return tok.type == TOKEN_LBRACE ? parse_init_values() : parse_expression();
}

static Expr *parse_init_value_and_optional_eos()
{
    if (tok.type == TOKEN_LBRACE) return parse_init_values();

    Expr *value = parse_expression();

	CONSUME_REQ_EOS_AND_RETURN(value);
}

/**
 * declaration ::= type variable
 *                 declaration '=' expression
 * @return Decl *or NULL if error is found.
 */
static Decl *parse_declaration()
{

	Token start = tok;

	Type *type_expr = parse_type(true, false);
	if (!type_expr) return NULL;

    Decl *declaration = new_var_decl(&start, &tok, VARDECL_LOCAL, type_expr, false);

	if (!consume(TOKEN_IDENTIFIER, "Expected variable name")) return NULL;

    declaration->name = prev_tok;

    Expr *init_expr = try_consume(TOKEN_EQ) ? parse_init_value() : NULL;

	declaration->var.init_expr = init_expr;

	UPDATE_AND_RETURN(declaration);
}

/**
 * label_stmt := IDENTIFIER ':'
 * @return Ast*
 */
static Ast *parse_label_stmt()
{
	advance_and_verify(TOKEN_IDENTIFIER);
	Ast *ast = new_ast_with_span(AST_LABEL, &prev_tok);
	ast->label_stmt.label_name = prev_tok;
	ast->label_stmt.is_used = false;
	ast->label_stmt.defer_top = NULL;
	// Already checked!
	advance_and_verify(TOKEN_COLON);

	UPDATE_AND_RETURN(ast);
}

/**
 * declaration_stmt ::= declaration EOS
 * @return Ast * or null if error is found
 */
static Ast *parse_declaration_stmt()
{
	Decl *declaration = parse_declaration();
	if (!declaration) return NULL;
	if (!declaration->var.init_expr
	    || declaration->var.init_expr->expr_id != EXPR_STRUCT_INIT_VALUES)
	{
		CONSUME_EOS_OR_EXIT;
	}
	Ast *decl_stmt = new_ast_with_span(AST_DECLARE_STMT, &declaration->span);
	decl_stmt->declare_stmt.decl = declaration;
	UPDATE_AND_RETURN(decl_stmt);
}

static inline int skip_array(int lookahead_steps)
{
	int bracket = 1;
	while (1)
	{
		switch (lookahead(++lookahead_steps).type)
		{
			case TOKEN_LBRACKET:
				bracket++;
				break;
			case TOKEN_RBRACKET:
				bracket--;
				if (bracket == 0) return lookahead_steps;
				break;
			case TOKEN_ERROR:
			case TOKEN_EOF:
				return lookahead_steps;
			default:
				break;
		}
	}
}
static inline bool is_next_decl()
{
	/*
		 Declarations (type + name)
	       a* b <init> -> yes
	       a[] b <init> -> yes
	       a[10] b -> yes
	       a*[] b <init> -> yes
	       a b <init> -> yes
	       a.b c -> yes
	     Assignments/Function calls
	       a = ..     -> no
	       a *= .. etc -> no
	       a() -> no
	       a[10] = .. -> no
	       a.b.c .. -> no
	 */
	enum _state
	{
		STATE_IDENT,
		STATE_POINTERS,
		STATE_ARRAY
	};
	int lookahead_steps = 0;
	enum _state state = STATE_IDENT;
	while (1)
	{
		switch (lookahead(++lookahead_steps).type)
		{
			case TOKEN_DOT:
				// Expect period + identifier
				if (state == STATE_IDENT)
				{
					if (lookahead(++lookahead_steps).type != TOKEN_IDENTIFIER) return false;
					state = STATE_POINTERS;
					break;
				}
				// Just go to false otherwise.
				return false;
			case TOKEN_IDENTIFIER:
				return true;
			case TOKEN_STAR:
				// a[] * ... => not a declaration
				if (state == STATE_ARRAY) return false;
				state = STATE_POINTERS;
				break;
			case TOKEN_LBRACKET:
				lookahead_steps = skip_array(lookahead_steps);
				state = STATE_ARRAY;
				break;
			default:
				return false;
		}
    }
}


/*
  Syntax:
    Number num = .     // id = type
    Utils.Type t = .  // id = module.type
    myfunc()        // id = func
    Mod.func()     // id = module.func
    count =         // id = var
    Mod.var =      // id = module.var
    id:             // id = label
*/
static Ast *parse_decl_or_stmt()
{
 	assert(current_is(TOKEN_IDENTIFIER) && "Not identifier");
 	if (lookahead(1).type == TOKEN_COLON)
	{
		return parse_label_stmt();
	}
    if (is_next_decl())
	{
    	return parse_declaration_stmt();
	}

    return parse_expr_stmt();
}

/**
 * for_stmt ::= FOR '(' expr[opt] ';' expr[opt] ';' expr[opt] ')' statement
 *              FOR '(' declaration expr[opt] ';' expr[opt] ';' expr[opt] ')' statement
 * @return Ast*
 */
static Ast *parse_for_stmt()
{
	advance_and_verify(TOKEN_FOR);
	Ast *for_ast = new_ast_with_span(AST_FOR_STMT, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Expected '(' after for")) return NULL;

	Ast *init = NULL;
	if (!try_consume(TOKEN_EOS))
	{
		init = parse_decl_or_stmt();
		if (!init) return NULL;
	}

	Expr *cond = NULL;
	if (!try_consume(TOKEN_EOS))
	{
		Ast *cond_stmt = parse_expr_stmt();
		if (!cond_stmt) return NULL;
		cond = cond_stmt->expr_stmt.expr;
	}

	Expr *incr = NULL;
	if (!try_consume(TOKEN_RPAREN))
	{
		incr = parse_expression();
		if (!incr) return NULL;
		if (!consume(TOKEN_RPAREN, "Expected ')' to end for")) return NULL;
	}
	Ast *body = stmt_to_compound(parse_stmt());
	for_ast->for_stmt.init = init;
	for_ast->for_stmt.cond = cond;
	for_ast->for_stmt.incr = incr;
	for_ast->for_stmt.body = body;
	UPDATE_AND_RETURN(for_ast);
}


static Ast *parse_stmt(void)
{
	switch (tok.type)
	{
		case TOKEN_LBRACE:
			return parse_compound_stmt();
		case TOKEN_IF:
			return parse_if_stmt();
		case TOKEN_WHILE:
			return parse_while_stmt();
		case TOKEN_DEFER:
			return parse_defer_stmt();
		case TOKEN_SWITCH:
			return parse_switch_stmt();
		case TOKEN_GOTO:
			return parse_goto_stmt();
		case TOKEN_DO:
			return parse_do_stmt();
		case TOKEN_FOR:
			return parse_for_stmt();
		case TOKEN_CONTINUE:
			return parse_continue_stmt();
		case TOKEN_CASE:
			error_at_current("Case not in switch");
			return NULL;
		case TOKEN_BREAK:
			return parse_break_stmt();
		case TOKEN_RETURN:
			return parse_return_stmt();
		case TOKEN_CONST:
		case TOKEN_VOLATILE:
			return parse_declaration_stmt();
		case TOKEN_IDENTIFIER:
			return parse_decl_or_stmt();
		case TOKEN_ASM:
			return parse_asm_stmt();
		case TOKEN_DEFAULT:
			error_at_current("Default not in switch");
			return NULL;
		case TOKEN_STAR:
			return parse_expr_stmt();
		case TOKEN_RBRACE:
			error_at_current("Expected statement here");
		default:
			return parse_expr_stmt();
	}
}


/**
 * imports ::= import
 *           | imports import
 */
void parse_imports()
{
	while (tok.type == TOKEN_IMPORT)
	{
		Decl *import = parse_import();
		if (import) vector_add(current_parser->imports, import);
	}
}

/**
 * struct_block ::= '{' (struct_member EOS)* '}'
 *
 * struct_member ::= type_qualifier type_specifier
 *                 | STRUCT IDENTIFIER[opt] struct_block
 *                 | UNION IDENTIFIER[opt] struct_block
 * @param struct_members the vector to add the struct members to.
 */
bool parse_struct_block(Decl *struct_members)
{
	if (!consume(TOKEN_LBRACE, "Expected '{' to begin definition")) return false;

	struct_members->struct_decl.members = new_vector(4);

	while (true)
	{
		if (tok.type == TOKEN_RBRACE) break;


		if (tok.type == TOKEN_STRUCT || tok.type == TOKEN_UNION)
		{
			Decl *member = decl_new(DECL_STRUCT_TYPE, &tok, &tok, struct_members->is_public);
			member->struct_decl.struct_type = tok.type == TOKEN_STRUCT ? ST_STRUCT : ST_UNION;
			advance();
			if (try_consume(TOKEN_IDENTIFIER))
			{
				member->name = prev_tok;
			}
			else
			{
				member->name.length = 0;
			}
			if (!parse_struct_block(member)) return false;
			token_expand(&member->span, &prev_tok);
			vector_add(struct_members->struct_decl.members, member);
			continue;
		}
		Type *type = parse_type(true, struct_members->is_public);
		if (!type) return false;
		Decl *member = new_var_decl(&tok, &tok, VARDECL_MEMBER, type, struct_members->is_public);

		if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return false;
		member->name = prev_tok;
		token_expand(&member->span, &prev_tok);
		if (!consume(TOKEN_EOS, "Expected ';'")) return false;
	}
	if (!consume(TOKEN_RBRACE, "Expected '}'")) return false;
    return true;

}

/**
 * struct_or_union_type ::= UNION struct_block attributes?
 *                        | STRUCT struct_block attributes?
 * @param public if the function is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Decl* or NULL in case of error
 */
static Decl *parse_struct_or_union_type(bool public, Token *initial_token)
{

	bool is_struct = tok.type == TOKEN_STRUCT;
	Decl *struct_decl = decl_new(DECL_STRUCT_TYPE, initial_token, &prev_tok, public);

	struct_decl->struct_decl.struct_type = is_struct ? ST_STRUCT : ST_UNION;
	struct_decl->struct_decl.is_global = true;

    advance();

    struct_decl->struct_decl.members = new_vector(8);
	if (!parse_struct_block(struct_decl)) return NULL;
	if (!optional_add_attributes(&struct_decl->attributes)) return NULL;

	UPDATE_AND_RETURN(struct_decl);
}


/**
 * alias_type ::= IDENTIFIER type_specifier

 * @param public if the alias is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Decl* or NULL in case of error
 */
static Decl *parse_alias_type(bool public, Token *initial_token)
{
	Decl *alias = decl_new(DECL_ALIAS_TYPE, initial_token, &prev_tok, public);
	Type *type = parse_type(true, public);
	if (!type) return NULL;
	alias->alias_decl.type = type;

    // TODO parse attributes?
	CONSUME_REQ_EOS_AND_RETURN(alias);
}

/**
 * func_type ::= FUNC type_qualifier single_type_qualifier '(' full_param_list ')' attributes?
 * @param public if the function is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Decl* or NULL in case of error
 */
static Decl *parse_function_type(bool public, Token *initial_token)
{
	Decl *func_decl = parse_func_decl(public, true);
	if (!func_decl) return NULL;

	Decl *type = decl_new(DECL_FUNC_TYPE, initial_token, &prev_tok, public);
	type->func_type.func_decl = func_decl;

	CONSUME_REQ_EOS_AND_RETURN(type);
}

/**
 * enum_type ::= ENUM type '{' '+'[opt] enum_block '}' attributes?
 *
 * enum_block ::= enum_member
 *                enum_block ','
 *                enum_block ',' enum_member
 *
 * enum_member ::= IDENTIFIER
 *               | IDENTIFIER '=' expression
 *
 * @param public if the enum is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Ast* or NULL in case of error
 */
static Decl *parse_enum_type(bool public, Token *initial_token)
{
	Decl *enum_decl = decl_new(DECL_ENUM_TYPE, initial_token, &prev_tok, public);

	advance_and_verify(TOKEN_ENUM);

	Expr *type = parse_full_identifier();

	if (!type) return NULL;

	enum_decl->enum_decl.type = new_unresolved_type(type, public);

    CONSUME_START_BRACE_OR_EXIT;

    enum_decl->enum_decl.incremental = try_consume(TOKEN_PLUS);

    Vector *entries = new_vector(8);
    enum_decl->enum_decl.constants = entries;

	while (1)
    {
	    if (!consume(TOKEN_IDENTIFIER, "Expected enum name")) return NULL;
	    Decl *enum_constant = decl_new(DECL_ENUM_CONSTANT, &prev_tok, &prev_tok, public);
	    enum_constant->type.decl = enum_decl;
	    vector_add(entries, enum_constant);
        vector_add(current_parser->enum_values, enum_constant);
        if (try_consume(TOKEN_EQ))
        {
            Expr *value = parse_expression();
            if (!value) return NULL;
            enum_constant->enum_constant.init_value = value;
        }
        // IMPROVE add values to enum
        if (!try_consume(TOKEN_COMMA))
		{
			CONSUME_END_BRACE_OR_EXIT;
            break;
		}
        if (try_consume(TOKEN_RBRACE)) break;
    }

	if (has_attributes())
    {
        if (!(enum_decl->attributes = parse_attributes())) return NULL;
    }

    UPDATE_AND_RETURN(enum_decl);
}

/**
 * type_definition ::= TYPE IDENTIFIER type_qualifier type_specifier
 *                   | TYPE IDENTIFIER func_type
 *                   | TYPE IDENTIFIER struct_or_union_type
 *                   | TYPE IDENTIFIER ENUM '{' enum_block '}'
 * @param public if the type is public or not.
 * @return Ast * or NULL if parsing fails.
 */
static Decl *parse_type_definition(bool public)
{
    Token initial_token = public ? prev_tok : tok;

	advance_and_verify(TOKEN_TYPE);

	if (!consume(TOKEN_IDENTIFIER, "Expected identifier after 'type'")) return NULL;

	switch (tok.type)
	{
		case TOKEN_FUNC:
			return parse_function_type(public, &initial_token);
		case TOKEN_STRUCT:
		case TOKEN_UNION:
            return parse_struct_or_union_type(public, &initial_token);
		case TOKEN_ENUM:
			return parse_enum_type(public, &initial_token);
	    default:
	        return parse_alias_type(public, &initial_token);
	}
}

/**
 * func_definition ::= function_type compund_statement
 *
 * @param public is the function is public
 * @return Decl *with the function defintion or NULL if parsing fails.
 */
static Decl *parse_func_definition(bool public)
{
    Decl *decl = parse_func_decl(public, false);
    if (!decl) return NULL;

    if (current_parser->is_interface)
	{
    	CONSUME_EOS_OR_EXIT;
		UPDATE_AND_RETURN(decl);
	}
    Ast *body = parse_compound_stmt();
    if (!body) return NULL;
    decl->func_decl.body = body;

    UPDATE_AND_RETURN(decl);
}

/**
 * variable_definition ::= public[opt] type_specifier IDENTIFIER attributes[opt] variable_initialization
 *
 * variable_initialization ::= '=' expression EOS
 *                           | '=' init_values
 *                           | EOS
 *
 * @param public if it is public or not
 */
static Decl *parse_var_definition(bool public)
{
	Token start = public ? prev_tok : tok;

    Type *type = parse_type(true, public);
    if (!type) return NULL;

    if (!consume(TOKEN_IDENTIFIER, "Expected variable name")) return NULL;

	Decl *decl = new_var_decl(&start, &prev_tok, VARDECL_GLOBAL, type, public);
	decl->is_public = public;

	if (!optional_add_attributes(&decl->attributes)) return NULL;

    if (try_consume(TOKEN_EQ))
    {
        Expr *init_expr = parse_init_value_and_optional_eos();
        if (!init_expr) return NULL;
        decl->var.init_expr = init_expr;
    }
    else
    {
        CONSUME_EOS_OR_EXIT;
    }

    UPDATE_AND_RETURN(decl);

}


/**
 * array_entry ::= IDENTIFIER '+=' init_value
 */
Decl *parse_array_entry()
{
	advance_and_verify(TOKEN_IDENTIFIER);

	Decl *array = decl_new(DECL_ARRAY_VALUE, &prev_tok, &prev_tok, false);

	// TODO set module?

	advance_and_verify(TOKEN_PLUS_ASSIGN);

	Expr *result = parse_init_value();

	if (!result) return NULL;

	array->array_decl.value = result;

	if (result->expr_id != EXPR_STRUCT_INIT_VALUES)
	{
	    CONSUME_EOS_OR_EXIT;
	}

	vector_add(current_parser->array_values, array);

    UPDATE_AND_RETURN(array);
}

/**
 * top_level ::= top_level
 *             | top_level top_level_item
 *
 * top_level_item ::= array_entry                     -> IDENTIFIER += init_value
 *                  | public[opt] type_definition     -> [public] type IDENTIFIER ...
 *                  | public[opt] function            -> [public] func ...
 *                  | public[opt] var_definition      -> [public] type_qualifier IDENTIFIER
 */
static void parse_top_level()
{
    // IMPROVE: error recovery
	while (tok.type != TOKEN_EOF)
	{
		if (error_found()) return;
		if (tok.type == TOKEN_IDENTIFIER && lookahead(1).type == TOKEN_PLUS_ASSIGN)
		{
			Decl *entry = parse_array_entry();
            if (entry) vector_add(current_parser->variables, entry);
			continue;
		}
		bool public = try_consume(TOKEN_PUBLIC);
		Decl *decl;
		switch (tok.type)
		{
			case TOKEN_TYPE:
				decl = parse_type_definition(public);
				if (decl) vector_add(current_parser->types, decl);
				break;
			case TOKEN_IMPORT:
				error_at_current("Import found outside start of file.");
				recover_to(TOKEN_EOS);
				break;
			case TOKEN_MODULE:
				error_at_current("Module declaration needs to be at the start of the file.");
				parse_module();
				break;
			case TOKEN_FUNC:
                decl = parse_func_definition(public);
                if (decl) vector_add(current_parser->functions, decl);
				break;
			default:
                decl = parse_var_definition(public);
                if (decl) vector_add(current_parser->variables, decl);
				break;
		}
	}
}
/**
 * source ::= module import top_level
 * @return false if error occurs;
 */
bool parse_source()
{
	parse_module();
	parse_imports();
	parse_top_level();
	if (!error_found())
	{
		consume(TOKEN_EOF, "Expected end of file.");
	}

	return !error_found();
}



/**
 * If we find: expr*[ or expr*) or expr*; then assume this is an identifier.
 *
 * @param left_side
 * @return true if this should be a type expression.
 */
static bool is_type_expr_after_star(Expr *left_side)
{
	if (left_side->expr_id == EXPR_TYPE) return true;
	switch (tok.type)
	{
		case TOKEN_RPAREN:
		case TOKEN_EOS:
		case TOKEN_STAR:
		case TOKEN_LBRACKET:
			return true;
		default:
			return false;
	}
}
static Expr *parse_binary(Expr *left_side)
{

	// Remember the operator.
	token_type operator_type = prev_tok.type;

	// There is a possibility that we actually have a type here.
	if (operator_type == TOKEN_STAR && is_type_expr_after_star(left_side))
	{
		Type *type = new_type(TYPE_POINTER, false);
		type->pointer.base = new_unresolved_type(left_side, false);
		Expr *expr = expr_new_type_expr(type, &left_side->span);
		UPDATE_AND_RETURN(expr);
	}
	ParseRule *rule = get_rule(operator_type);
	Expr *right_side = parse_precedence(rule->precedence + 1);

	Expr *binary = expr_new(EXPR_BINARY, &left_side->span);

	binary->binary_expr.left = left_side;
	binary->binary_expr.right = right_side;
	binary->binary_expr.operator = operator_type;

	UPDATE_AND_RETURN(binary);

}

static Expr *parse_ternary(Expr *left_side)
{

	Expr *expr_ternary = expr_new(EXPR_TERNARY, &left_side->span);
	expr_ternary->ternary_expr.cond = left_side;

	// Check for elvis
	if (tok.type == TOKEN_COLON)
	{
		expr_ternary->ternary_expr.then_expr = NULL;
	}
	else
	{
		Expr *true_expr = parse_precedence(PREC_TERNARY + 1);
		if (!true_expr) return NULL;
		expr_ternary->ternary_expr.then_expr = true_expr;
	}

	if (!consume(TOKEN_COLON, "Expected ':'")) return NULL;

	Expr *false_expr = parse_precedence(PREC_TERNARY + 1);

	if (!false_expr) return NULL;

	expr_ternary->ternary_expr.else_expr = false_expr;
	UPDATE_AND_RETURN(expr_ternary);
}



// Right associative
static Expr *parse_binary_right(Expr *left_side)
{
	// Remember the operator.
	token_type operator_type = prev_tok.type;

	ParseRule *rule = get_rule(operator_type);
	Expr *right_side = parse_precedence((Precedence)(rule->precedence));

	Expr *binary = expr_new(EXPR_BINARY, &left_side->span);
	binary->binary_expr.left = left_side;
	binary->binary_expr.right = right_side;
	binary->binary_expr.operator = operator_type;
	UPDATE_AND_RETURN(binary);

}

static Expr *parse_unary(Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	Expr *unary = expr_new(EXPR_UNARY, &prev_tok);
	unary->unary_expr.operator = prev_tok.type;
	unary->unary_expr.expr = parse_precedence(PREC_UNARY);
	UPDATE_AND_RETURN(unary);
}

static Expr *parse_post_unary(Expr *left)
{
	Expr *unary = expr_new(EXPR_POST, &left->span);
	unary->post_expr.operator = prev_tok.type;
	unary->post_expr.expr = left;
	UPDATE_AND_RETURN(unary);
}

static Expr *parse_grouping(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *ast = parse_expression();
	consume(TOKEN_RPAREN, "Expected ')' after expression.");
	return ast;
}

static Expr *parse_call(Expr *left)
{
	if (!left)
	{
		return NULL;
	}
	Vector *vector;
	if (try_consume(TOKEN_RPAREN))
	{
		vector = new_vector(0);
	}
	else
	{
		vector = new_vector(4);
		do
		{
			Expr *param = parse_expression();
			if (!param) return NULL;
			vector_add(vector, param);
		} while (try_consume(TOKEN_COMMA));
		if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	}
	Expr *call = expr_new(EXPR_CALL, &left->span);
	call->call_expr.function = left;
	call->call_expr.parameters = vector;
	UPDATE_AND_RETURN(call);
}

static Expr *parse_sizeof(Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	Expr *expr = expr_new(EXPR_SIZEOF, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	expr->sizeof_expr.expr = parse_expression();
	if (!expr->sizeof_expr.expr) return NULL;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;

	UPDATE_AND_RETURN(expr);
}

/**
 * cast ::= CAST '(' expression ',' type_expr ')'
 *
 * @param left
 * @return
 */
static Expr *parse_cast(Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	Expr *expr = expr_new(EXPR_CAST, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	expr->cast_expr.expr = parse_expression();
	if (!expr->cast_expr.expr) return NULL;

	if (!consume(TOKEN_COMMA, "Expected ','")) return NULL;

	expr->cast_expr.type = parse_type(true, false);

	if (!expr->cast_expr.type) return NULL;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;

	UPDATE_AND_RETURN(expr);
}

static Expr *parse_subscript(Expr *left)
{
	if (!left)
	{
		return NULL;
	}
	// Check for possible array type, if so then this is a type expression.
	if (try_consume(TOKEN_RBRACKET))
	{
		Type *type = new_type(TYPE_ARRAY, false);
		type->array.base = new_unresolved_type(left, false);
		type->array.is_empty = true;
		type->array.is_len_resolved = true;
        Expr *expr = expr_new_type_expr(type, &left->span);
        UPDATE_AND_RETURN(expr);
	}
	Expr *index = parse_expression();
	consume(TOKEN_RBRACKET, "Expected ]");
	if (!index) return NULL;
    Expr *subscript_ast = expr_new(EXPR_SUBSCRIPT, &left->span);
	subscript_ast->subscript_expr.expr = left;
	subscript_ast->subscript_expr.index = index;
    UPDATE_AND_RETURN(subscript_ast);
}


static Expr *parse_access(Expr *left)
{
	if (!left) return NULL;
	Expr *accessed = parse_expression();
	Expr *access_expr = expr_new(EXPR_ACCESS, &left->span);
	access_expr->access_expr.parent = left;
	access_expr->access_expr.sub_element = accessed;
	UPDATE_AND_RETURN(access_expr);
}

static void set_parse_rule(token_type type, ParseFn prefix, ParseFn infix, Precedence rule_precedence)
{
	rules[type].prefix = prefix;
	rules[type].precedence = rule_precedence;
	rules[type].prefix = prefix;
	rules[type].infix = infix;
};


void setup_parse_rules(void)
{
	static bool parse_rules_done = false;
	if (parse_rules_done) return;
	set_parse_rule(TOKEN_QUESTION, NULL, parse_ternary, PREC_TERNARY);
	set_parse_rule(TOKEN_PLUSPLUS, parse_unary, parse_post_unary, PREC_CALL);
	set_parse_rule(TOKEN_MINUSMINUS, parse_unary, parse_post_unary, PREC_CALL);
	set_parse_rule(TOKEN_LPAREN, parse_grouping, parse_call, PREC_CALL);
	set_parse_rule(TOKEN_CAST, parse_cast, NULL, PREC_NONE);
	set_parse_rule(TOKEN_SIZEOF, parse_sizeof, NULL, PREC_NONE);
	set_parse_rule(TOKEN_LBRACKET, NULL, parse_subscript, PREC_CALL);
	set_parse_rule(TOKEN_MINUS, parse_unary, parse_binary, PREC_TERM);
	set_parse_rule(TOKEN_PLUS, NULL, parse_binary, PREC_TERM);
	set_parse_rule(TOKEN_DIV, NULL, parse_binary, PREC_FACTOR);
	set_parse_rule(TOKEN_MOD, NULL, parse_binary, PREC_FACTOR);
	set_parse_rule(TOKEN_STAR, parse_unary, parse_binary, PREC_FACTOR);
	set_parse_rule(TOKEN_DOT, NULL, parse_access, PREC_CALL);
	set_parse_rule(TOKEN_NOT, parse_unary, NULL, PREC_NONE);
	set_parse_rule(TOKEN_BIT_NOT, parse_unary, NULL, PREC_NONE);
	set_parse_rule(TOKEN_BIT_XOR, NULL, parse_binary, PREC_BITWISE);
	set_parse_rule(TOKEN_BIT_OR, NULL, parse_binary, PREC_BITWISE);
	set_parse_rule(TOKEN_AMP, parse_unary, parse_binary, PREC_BITWISE);
	set_parse_rule(TOKEN_EQEQ, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_NOT_EQUAL, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_GREATER, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_GREATER_EQ, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS_EQ, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LEFT_SHIFT, NULL, parse_binary, PREC_SHIFT);
	set_parse_rule(TOKEN_RIGHT_SHIFT, NULL, parse_binary, PREC_SHIFT);
	set_parse_rule(TOKEN_TRUE, parse_bool, NULL, PREC_NONE);
	set_parse_rule(TOKEN_FALSE, parse_bool, NULL, PREC_NONE);
	set_parse_rule(TOKEN_NIL, parse_nil, NULL, PREC_NONE);
	set_parse_rule(TOKEN_INTEGER, parse_integer, NULL, PREC_NONE);
	set_parse_rule(TOKEN_IDENTIFIER, parse_identifier, NULL, PREC_NONE);
	set_parse_rule(TOKEN_STRING, parse_string_literal, NULL, PREC_NONE);
	set_parse_rule(TOKEN_POW, NULL, parse_binary_right, PREC_POW);
	set_parse_rule(TOKEN_FLOAT, parse_double, NULL, PREC_NONE);
	set_parse_rule(TOKEN_OR, NULL, parse_binary, PREC_OR);
	set_parse_rule(TOKEN_AND, NULL, parse_binary, PREC_OR);
	set_parse_rule(TOKEN_EQ, NULL, parse_binary, PREC_ASSIGNMENT);

	set_parse_rule(TOKEN_PLUS_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_MINUS_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_MULT_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_MOD_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_DIV_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_AND_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_OR_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_BIT_XOR_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_BIT_AND_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_BIT_OR_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_RIGHT_SHIFT_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
	set_parse_rule(TOKEN_LEFT_SHIFT_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);

    parse_rules_done = true;
}


Parser *parse(const char *filename, bool is_interface)
{
    init_lexer(filename, read_file(filename));
	Parser *parser = malloc_arena(sizeof(Parser));
	init_parser(parser, filename, is_interface);
    current_parser = parser;
	advance();
	parse_source();
	current_parser = NULL;
	if (error_found())
	{
		return NULL;
	}
	return parser;
}

