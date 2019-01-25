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

Parser *current_parser;
Token tok;
Token prev_tok;

#define CONSUME_EOS_OR_EXIT if (!consume(TOKEN_EOS, "Expected ';'")) return NULL
#define CONSUME_START_BRACE_OR_EXIT if (!consume(TOKEN_LBRACE, "Expected '{'")) return NULL
#define CONSUME_END_BRACE_OR_EXIT if (!consume(TOKEN_RBRACE, "Expected '}'")) return NULL
#define UPDATE_AND_RETURN_AST(__AST) return end_ast(__AST, &prev_tok)

static inline bool has_attributes()
{
    return tok.type == TOKEN_AT;
}

static Ast *stmt_to_compound(Ast *inner_ast)
{
	if (!inner_ast || inner_ast->type == AST_COMPOUND_STMT) return inner_ast;
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


typedef Ast *(*ParseFn)(Ast *);

typedef enum
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
} precedence;

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	precedence precedence;
} ParseRule;


static ParseRule rules[TOKEN_EOF + 1];


static Ast *parse_expression(void);
static Ast *parse_stmt(void);
static Ast *parse_init_value(void);


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
	vprint_error(&tok, message, args);
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
static Ast *parse_import()
{
	advance_and_verify(TOKEN_IMPORT);
    Ast *ast = new_ast_with_span(AST_IMPORT, &prev_tok);
    ast->import.used = false;
    ast->import.used_public = false;
    ast->import.alias.length = 0;
    ast->import.alias.type = TOKEN_VOID;
    ast->import.module = NULL;
	if (!consume(TOKEN_IDENTIFIER, "Expected an identifier"))
	{
		recover_to(TOKEN_EOS);
		return NULL;
	}

	ast->import.module_name = prev_tok;
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
		ast->import.alias = prev_tok;
		ast->import.type = IMPORT_TYPE_ALIAS;
	}
	else if (try_consume(TOKEN_LOCAL))
	{
        ast->import.type = IMPORT_TYPE_LOCAL;
	}
	CONSUME_EOS_OR_EXIT;

	if (token_compare(&ast->import.module_name, &current_parser->current_module))
   	{
   		error_at(&prev_tok, "Cannot import the current module");
        return NULL;
   	}
    UPDATE_AND_RETURN_AST(ast);
}



/**
 * module ::= MODULE IDENTIFIER EOS
 */
Ast *parse_module()
{
	// No module? Error?
	if (!try_consume(TOKEN_MODULE))
	{
		error_at_current("Expected 'module' at the start of file");
		return NULL;
	}
	Ast *import = new_ast_with_span(AST_IMPORT, &prev_tok);
	import->import.used = true;
	import->import.used_public = false;
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
	import->import.type = IMPORT_TYPE_LOCAL;
	import->import.module_name = prev_tok;
	current_parser->current_module = prev_tok;
	CONSUME_EOS_OR_EXIT;
	vector_add(current_parser->imports, import);
	UPDATE_AND_RETURN_AST(import);
}


static Ast *parse_string_literal(Ast *left)
{
	assert(!left && "Had left hand side");
	Ast *ast_string = new_ast_with_span(AST_STRING_EXPR, &prev_tok);
	ast_string->string_expr.string = prev_tok;
	// Just keep chaining if there are multiple parts.
	ast_string->string_expr.next_string = try_consume(TOKEN_STRING) ? parse_string_literal(NULL) : NULL;
	ast_string->const_state = CONST_FULL;
	return ast_string;
}

static Ast *parse_integer(Ast *left)
{
	assert(!left && "Had left hand side");
	Ast *number = new_ast_with_span(AST_UINT_EXPR, &tok);
	number->uint_expr.u = parse_uint64(prev_tok.start, prev_tok.length);
	number->const_state = CONST_FULL;
	return number;
}


static Ast *parse_double(Ast *left)
{
	assert(!left && "Had left hand side");
	Ast *number = new_ast_with_span(AST_FLOAT_EXPR, &tok);
	number->const_state = CONST_FULL;
	// TODO handle error
	number->float_expr.f = strtod(prev_tok.start, NULL);
	return number;
}

static Ast *parse_bool(Ast *left)
{
	assert(!left && "Had left hand side");
	Ast *number = new_ast_with_span(AST_BOOL_EXPR, &tok);
	number->const_state = CONST_FULL;
	number->bool_expr.i = tok.type == TOKEN_TRUE;
	return number;
}

static Ast *parse_nil(Ast *left)
{
	assert(!left && "Had left hand side");
	Ast *value = new_ast_with_span(AST_NIL_EXPR, &tok);;
	value->const_state = CONST_FULL;
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
Ast *parse_attributes()
{
	advance_and_verify(TOKEN_AT);

	Ast *attribute_list = new_ast_with_span(AST_ATTRIBUTE_LIST, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Missing '(' after '@'")) return NULL;

	attribute_list->attribute_list.list = new_vector(4);

	while (true)
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
        Ast *attribute = new_ast_with_span(AST_ATTRIBUTE, &prev_tok);
        attribute->attribute.name = prev_tok;
        Ast *arg = NULL;
		if (try_consume(TOKEN_EQ))
		{
			arg = parse_expression();
		}
		attribute->attribute.value = arg;
        vector_add(attribute_list->attribute_list.list, attribute);
		if (!try_consume(TOKEN_COMMA)) break;
	}
	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;
	return end_ast(attribute_list, &prev_tok);
}

static bool optional_add_attributes(Ast **entry)
{
    if (tok.type != TOKEN_AT)
    {
        *entry = NULL;
        return true;
    }
    Ast *attributes = parse_attributes();
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
static Ast *parse_identifier(Ast *left)
{
	assert(!left && "Unexpected left hand side");

	Ast *ast = new_ast_with_span(AST_IDENTIFIER_EXPR, &prev_tok);
	ast->identifier_expr.identifier = prev_tok;
	return ast;
}

/**
 * full_identifier ::= IDENTIFIER
 *                     IDENTIFIER '.' IDENTIFIER
 * @return the resulting identifier or NULL if an error occurs.
 */
Ast *parse_full_identifier()
{
	if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
	Ast *ast = new_type_expr(TYPE_EXPR_IDENTIFIER, &prev_tok);

	ast->type_expr.identifier_type_expr.resolved_type = NULL;
	Token first_name = prev_tok;
	if (try_consume(TOKEN_DOT))
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected identifier after ','")) return NULL;
		ast->type_expr.identifier_type_expr.module_name = first_name;
		ast->type_expr.identifier_type_expr.name = prev_tok;
	}
	else
	{
		ast->type_expr.identifier_type_expr.module_name.length = 0;
		ast->type_expr.identifier_type_expr.name = first_name;
	}
	UPDATE_AND_RETURN_AST(ast);
}



static Ast *parse_precedence_after_advance(precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = get_rule(prev_tok.type)->prefix;
	if (prefix_rule == NULL)
	{
		error("Expected expression.");
		return NULL;
	}

	Ast *left_side = prefix_rule(NULL);
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

static void add_type_specifier(Ast *ast, TypeQualifier type_specifier)
{
	assert(ast->type == AST_TYPE_EXPR && "Expected type expression");
	if (!type_specifier) return;
	if (type_specifier & TYPE_QUALIFIER_CONST)
	{
		ast->type_expr.flags.const_ref = true;
	}
	if (type_specifier & TYPE_QUALIFIER_VOLATILE)
	{
		ast->type_expr.flags.volatile_ref = true;
	}
	if (type_specifier & TYPE_QUALIFIER_ALIAS)
	{
		ast->type_expr.flags.alias_ref = true;
	}
}

static Ast *create_pointer_type(Ast *base)
{
	Ast *array = new_type_expr(TYPE_EXPR_POINTER, &base->span);
	array->type_expr.pointer_type_expr.type = base;
	UPDATE_AND_RETURN_AST(array);
}

/**
 * array_type ::= '[' expression ']'
 *              | '[' ']'
 * @param base the base type
 * @return the resulting type or NULL if parsing failed.
 */
Ast *parse_array_type(Ast *base)
{
	assert(base && "Cannot have null expression");
	advance_and_verify(TOKEN_LBRACKET);
	Ast *array = new_type_expr(TYPE_EXPR_ARRAY, &base->span);
	array->type_expr.array_type_expr.type = base;
	if (try_consume(TOKEN_RBRACKET))
	{
		array->type_expr.array_type_expr.size = NULL;
		return array;
	}
	Ast *expression = parse_expression();
	if (!expression) return NULL;
	if (!consume(TOKEN_RBRACKET, "Missing ']'")) return NULL;
	array->type_expr.array_type_expr.size = expression;
	UPDATE_AND_RETURN_AST(array);
}

/**
 * type_specifier ::= full_identifier const?
 *                  | type_qualifier array_specifier const?
 *                  | type_qualifier '*' const?
 *                  |
 * @param accept_qualifiers read optional type specifiers (volatile, const etc)
 * @return type
 */
Ast *parse_type(bool accept_qualifiers)
{
	// IMPROVE AST span.
	TypeQualifier type_qualifier = accept_qualifiers ? read_optional_type_specifiers() : TYPE_QUALIFIER_NONE;
	Ast *base;
	if (try_consume(TOKEN_VOID))
	{
		base = new_type_expr(TYPE_EXPR_VOID, &prev_tok);
	}
	else
	{
		base = parse_full_identifier();
		if (!base) return NULL;
	}
	add_type_specifier(base, type_qualifier);
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

static Ast *parse_expression(void)
{
	advance();
	return parse_precedence_after_advance(PREC_ASSIGNMENT);
}

static Ast *parse_precedence(precedence precedence)
{
	advance();
	return parse_precedence_after_advance(precedence);
}



/**
 * param_decl ::= param_decl
 *              | param_decl IDENT
 *              | param_decl IDENT '=' expr
 */
static inline Ast *parse_param_decl()
{
	Ast *type = parse_type(true);
	if (!type) return NULL;

	Ast *param_decl = new_ast_with_span(AST_PARAM_DECL, &type->span);
	param_decl->param_decl.type = type;
	param_decl->param_decl.name.type = TOKEN_VOID;
	param_decl->param_decl.default_value = NULL;
	if (!try_consume(TOKEN_IDENTIFIER))
	{
		return end_ast(param_decl, &prev_tok);
	}
	param_decl->param_decl.name = prev_tok;
	if (try_consume(TOKEN_EQEQ))
	{
		param_decl->param_decl.default_value = parse_expression();
		if (!param_decl->param_decl.default_value) return NULL;
	}

	UPDATE_AND_RETURN_AST(param_decl);
}

/**
 * param_list ::= '...'
 *                | param_decl
 *                | param_list ',' param_list
 *                ;
 * @return ast or NULL
 */
static inline Ast *parse_argument_list()
{
	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	Ast *param_list = new_ast(AST_PARAM_LIST);
	if (try_consume(TOKEN_RPAREN))
	{
		param_list->param_list.param_list = new_vector(0);
		param_list->param_list.variadic = false;
		return param_list;
	}
	Vector *args = new_vector(4);
	bool variadic = false;
	while (true)
	{
		if (try_consume(TOKEN_ELIPSIS))
		{
			variadic = true;
		}
		else
		{
			Ast *decl = parse_param_decl();
			if (!decl) return NULL;
			vector_add(args, decl);
		}
		if (!try_consume(TOKEN_COMMA)) break;
		if (variadic)
		{
			error_at_current("Variadic parameter followed by arguments");
			return NULL;
		}
	}
	param_list->param_list.param_list = args;
	param_list->param_list.variadic = variadic;
	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;
	UPDATE_AND_RETURN_AST(param_list);
}

static inline Ast *parse_func_decl()
{
	advance_and_verify(TOKEN_FUNC);

	Ast *ast = new_ast_with_span(AST_FUNC_DECL, &prev_tok);

	ast->func_decl.module = NULL;

	Ast *return_type = parse_type(true);
	if (!return_type) return NULL;

	ast->func_decl.r_type = return_type;

	if (!consume(TOKEN_IDENTIFIER, "Expected function name but got '%.*s'", tok.length, tok.start)) return NULL;

	ast->func_decl.name = malloc_arena(sizeof(FunctionName));
	Token name = prev_tok;
	if (try_consume(TOKEN_DOT))
	{
		if (consume(TOKEN_IDENTIFIER, "Expected struct function name but got '%.*s'", tok.length, tok.start)) return NULL;
		ast->func_decl.name->struct_name = name;
		ast->func_decl.name->function_name = prev_tok;
		char *full_name = malloc_arena(name.length + prev_tok.length + 2);
		snprintf(full_name, name.length + prev_tok.length + 1,
				"%.*s.%.*s", name.length, name.start, prev_tok.length, prev_tok.start);
		ast->func_decl.name->full_name = token_wrap(full_name);
	}
	else
	{
		ast->func_decl.name->struct_name.length = 0;
		ast->func_decl.name->function_name = name;
		ast->func_decl.name->full_name = name;
	}

	Ast *argument_list = parse_argument_list();
	if (!argument_list) return NULL;

	ast->func_decl.params = argument_list;

	if (!optional_add_attributes(&ast->func_decl.attributes)) return NULL;

    UPDATE_AND_RETURN_AST(ast);
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

	Ast *expr = parse_expression();

	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;

	// Prefer it in compound form.
	Ast *if_body = parse_stmt();
	if (!if_body) return NULL;
	if (if_body->type != AST_COMPOUND_STMT && current_is(TOKEN_ELSE))
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
	if_ast->if_stmt.if_body = stmt_to_compound(if_body);
	if_ast->if_stmt.expr = expr;
	UPDATE_AND_RETURN_AST(if_ast);
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
	Ast *expr = parse_expression();
	if (!expr) return NULL;
	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	Ast *body = parse_stmt();
	if (!body) return NULL;
	while_ast->while_stmt.body = stmt_to_compound(body);
	while_ast->while_stmt.expr = expr;
	UPDATE_AND_RETURN_AST(while_ast);
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
	Ast *expr = parse_expression();
	if (!expr) return NULL;
	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	if (!consume(TOKEN_EOS, "Expected ';'")) return NULL;
	do_ast->type = AST_DO_STMT;
	do_ast->do_stmt.body = body;
	do_ast->do_stmt.expr = expr;
	UPDATE_AND_RETURN_AST(do_ast);
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
	UPDATE_AND_RETURN_AST(defer_stmt);
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
	Ast *expression = parse_expression();
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
	UPDATE_AND_RETURN_AST(ast);
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
	UPDATE_AND_RETURN_AST(ast);
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

	Ast *expr = parse_expression();
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
	UPDATE_AND_RETURN_AST(ast);
}

/**
 * break_stmt ::= BREAK EOS
 * @return Ast * or NULL if there is an error.
 */
static Ast *parse_break_stmt()
{
	advance_and_verify(TOKEN_BREAK);
	Ast *ast = new_ast_with_span(AST_BREAK_STMT, &prev_tok);
	CONSUME_EOS_OR_EXIT;
	UPDATE_AND_RETURN_AST(ast);
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
		UPDATE_AND_RETURN_AST(ast);
	}
	Ast *expression = parse_expression();
	if (!expression) return NULL;
	ast->return_stmt.expr = expression;
	CONSUME_EOS_OR_EXIT;
	UPDATE_AND_RETURN_AST(ast);
}

/**
 * goto_stmt ::= GOTO expression EOS
 * @return Ast* or NULL if parsing failed.
 */
static Ast *parse_goto_stmt()
{
	advance_and_verify(TOKEN_GOTO);
	Ast *ast = new_ast_with_span(AST_GOTO_STMT, &prev_tok);

	Ast *expression = parse_expression();
	if (!expression) return NULL;
	ast->goto_stmt.label = expression;

	CONSUME_EOS_OR_EXIT;
	UPDATE_AND_RETURN_AST(ast);
}

/**
 * continue_stmt ::= CONTINUE EOS
 * @return Ast *
 */
static Ast *parse_continue_stmt()
{
	advance_and_verify(TOKEN_CONTINUE);
	Ast *ast = new_ast_with_span(AST_CONTINUE_STMT, &prev_tok);
	CONSUME_EOS_OR_EXIT;
	UPDATE_AND_RETURN_AST(ast);
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
	Ast *expression = parse_expression();
	if (!expression) return NULL;
	CONSUME_EOS_OR_EXIT;
	UPDATE_AND_RETURN_AST(expression);
}


/**
 * field_designator ::= '.' IDENTIFIER '=' expression
 *
 * @return Ast * if successfull, NULL on error.
 */
static Ast *parse_field_designator()
{
	advance_and_verify(TOKEN_DOT);
	Ast *ast = new_ast_with_span(AST_DESIGNATED_INITIALIZED_EXPR, &prev_tok);

	if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;

	if (!consume(TOKEN_EQ, "Expected '=' after identifier")) return NULL;

	Ast *init_value = parse_init_value();
	if (!init_value) return NULL;

	ast->designated_initializer_expr.expr = init_value;
	UPDATE_AND_RETURN_AST(ast);
}

/**
 * init_values ::= expression
 *               | field_designator
 *               | init_values ',' expression
 *               | init_values ',' field_designator
 *
 * @return Ast* or NULL if parsing fails.
 */
static Ast *parse_init_values()
{
	advance_and_verify(TOKEN_LBRACE);

	Ast *ast = new_ast_with_span(AST_STRUCT_INIT_VALUES_EXPR, &tok);
	if (try_consume(TOKEN_RBRACE))
	{
		ast->struct_init_values_expr.values = new_vector(0);
		UPDATE_AND_RETURN_AST(ast);
	}
	Vector *values = new_vector(4);
	ast->struct_init_values_expr.values = values;
	do
	{
		Ast *value = current_is(TOKEN_DOT) ? parse_field_designator() : parse_expression();
		if (!value) return NULL;
		vector_add(values, value);

	} while (try_consume(TOKEN_COMMA));
	CONSUME_END_BRACE_OR_EXIT;
	UPDATE_AND_RETURN_AST(ast);
}

/**
 * init_value ::= expression
 *              | init_values
 * @return Ast* or NULL if an error occurs.
 */
static Ast *parse_init_value(void)
{
	return tok.type == TOKEN_LBRACE ? parse_init_values() : parse_expression();
}

static Ast *parse_init_value_and_optional_eos()
{
    if (tok.type == TOKEN_LBRACE) return parse_init_values();

    Ast *value = parse_expression();

    CONSUME_EOS_OR_EXIT;

    UPDATE_AND_RETURN_AST(value);
}

/**
 * declaration ::= type variable
 *                 declaration '=' expression
 * @return Ast * or NULL if error is found.
 */
static Ast *parse_declaration()
{
	Ast *declaration = new_ast_with_span(AST_DECLARATION, &prev_tok);
	Ast *type = parse_type(true);
	if (type == NULL) return NULL;

	Token identifier = tok;
	if (!consume(TOKEN_IDENTIFIER, "Expected variable name")) return NULL;

	Ast *init_expr = try_consume(TOKEN_EQ) ? parse_init_value() : NULL;

	declaration->type = AST_DECLARATION;
	declaration->declaration.identifier = identifier;
	declaration->declaration.initExpr = init_expr;
	declaration->declaration.declType = type;
	return declaration;
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

	// Already checked!
	advance_and_verify(TOKEN_COLON);

	UPDATE_AND_RETURN_AST(ast);
}

/**
 * declaration_stmt ::= declaration EOS
 * @return Ast * or null if error is found
 */
static Ast *parse_declaration_stmt()
{
	Ast *declaration = parse_declaration();
	if (!declaration) return NULL;
	if (!declaration->declaration.initExpr
			|| declaration->declaration.initExpr->type != AST_STRUCT_INIT_VALUES_EXPR)
	{
		CONSUME_EOS_OR_EXIT;
	}
	UPDATE_AND_RETURN_AST(declaration);
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

	Ast *cond = NULL;
	if (!try_consume(TOKEN_EOS))
	{
		cond = parse_expr_stmt();
		if (!cond) return NULL;
	}

	Ast *incr = NULL;
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
	UPDATE_AND_RETURN_AST(for_ast);
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
		Ast *import = parse_import();
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
bool parse_struct_block(Vector *struct_members)
{
	if (!consume(TOKEN_LBRACE, "Expected '{' to begin definition")) return false;

	while (true)
	{
		if (tok.type == TOKEN_RBRACE) break;

        Ast *struct_member = new_ast_with_span(AST_STRUCT_MEMBER, &tok);
        vector_add(struct_members, struct_member);
		if (tok.type == TOKEN_STRUCT || tok.type == TOKEN_UNION)
		{
            struct_member->struct_member.type = tok.type == TOKEN_STRUCT
                    ? STRUCT_MEMBER_TYPE_STRUCT
                    : STRUCT_MEMBER_TYPE_UNION;
			advance();
			if (try_consume(TOKEN_IDENTIFIER))
			{
			    struct_member->struct_member.name = prev_tok;
			}
			else
			{
			    struct_member->struct_member.name.length = 0;
			    struct_member->struct_member.name.type = TOKEN_VOID;
			}
			struct_member->struct_member.members = new_vector(4);
			if (!parse_struct_block(struct_member->struct_member.members)) return false;
            end_ast(struct_member, &prev_tok);
			// IMPROVE: Improve error recovery.
		}
		else
		{
			Ast *type = parse_type(true);
			if (!type) return false;
			if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return false;
			struct_member->struct_member.type = STRUCT_MEMBER_TYPE_NORMAL;
            struct_member->struct_member.value_type = type;
            struct_member->struct_member.name = prev_tok;
			end_ast(struct_member, &prev_tok);
			if (!consume(TOKEN_EOS, "Expected ';'")) return false;
		}
	}
	if (!consume(TOKEN_RBRACE, "Expected '}'")) return false;
    return true;

}

/**
 * struct_or_union_type ::= UNION struct_block attributes?
 *                        | STRUCT struct_block attributes?
 * @param public if the function is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Ast* or NULL in case of error
 */
static Ast *parse_struct_or_union_type(bool public, Token *initial_token)
{
    Ast *struct_ast = new_type_definition(STRUCT_TYPE, &prev_tok, public, initial_token);

    struct_ast->definition.is_struct = tok.type == TOKEN_STRUCT;
    advance();

	struct_ast->definition.def_struct.members = new_vector(8);
	if (!parse_struct_block(struct_ast->definition.def_struct.members)) return NULL;
    if (has_attributes())
    {
        Ast *attributes = parse_attributes();
        if (!attributes) return NULL;
		struct_ast->definition.attributes = attributes;
    }

    UPDATE_AND_RETURN_AST(struct_ast);
}


/**
 * alias_type ::= IDENTIFIER type_specifier

 * @param public if the alias is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Ast* or NULL in case of error
 */
static Ast *parse_alias_type(bool public, Token *initial_token)
{
    Ast *alias_ast = new_type_definition(ALIAS_TYPE, &prev_tok, public, initial_token);

	Ast *type = parse_type(true);
	if (!type) return NULL;
	alias_ast->definition.def_alias.type_definition = type;

    // TODO parse attributes?
    CONSUME_EOS_OR_EXIT;

    UPDATE_AND_RETURN_AST(alias_ast);
}

/**
 * func_type ::= FUNC type_qualifier single_type_qualifier '(' full_param_list ')' attributes?
 * @param public if the function is public
 * @param initial_token the first token belonging to the parse.
 * @return the created Ast* or NULL in case of error
 */
static Ast *parse_function_type(bool public, Token *initial_token)
{
	Ast *type = new_type_definition(FUNC_TYPE, &prev_tok, public, initial_token);
	Ast *func_decl = parse_func_decl();

	if (!func_decl) return NULL;

	type->definition.def_func.func_decl = func_decl;

	CONSUME_EOS_OR_EXIT;

    UPDATE_AND_RETURN_AST(type);
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
static Ast *parse_enum_type(bool public, Token *initial_token)
{
    Ast *enum_definition = new_type_definition(ENUM_TYPE, &prev_tok, public, initial_token);

	advance_and_verify(TOKEN_ENUM);

	Ast *type = parse_full_identifier();

	if (!type) return NULL;

    enum_definition->definition.def_enum.type = type;

    CONSUME_START_BRACE_OR_EXIT;

    enum_definition->definition.is_incremental = try_consume(TOKEN_PLUS);

	Vector *entries = new_vector(8);
	enum_definition->definition.def_enum.entries = entries;

	while (1)
    {
	    if (!consume(TOKEN_IDENTIFIER, "Expected enum name")) return NULL;
        Ast *enum_entry = new_ast_with_span(AST_ENUM_ENTRY, &prev_tok);
        enum_entry->enum_entry.name = prev_tok;
        vector_add(entries, enum_entry);
        if (try_consume(TOKEN_EQ))
        {
            Ast *value = parse_expression();
            if (!value) return NULL;
            enum_entry->enum_entry.value = value;
        }
        else
        {
            enum_entry->enum_entry.value = NULL;
        }
        // IMPROVE add values to enum
        if (!try_consume(TOKEN_COMMA))
		{
			CONSUME_END_BRACE_OR_EXIT;
            break;
		}
        if (try_consume(TOKEN_RBRACE)) break;
    }


    enum_definition->definition.attributes = NULL;
	if (has_attributes())
    {
        if (!(enum_definition->definition.attributes = parse_attributes())) return NULL;
    }

    UPDATE_AND_RETURN_AST(enum_definition);
}

/**
 * type_definition ::= TYPE IDENTIFIER type_qualifier type_specifier
 *                   | TYPE IDENTIFIER func_type
 *                   | TYPE IDENTIFIER struct_or_union_type
 *                   | TYPE IDENTIFIER ENUM '{' enum_block '}'
 * @param public if the type is public or not.
 * @return Ast * or NULL if parsing fails.
 */
static Ast *parse_type_definition(bool public)
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
 * @return Ast *with the function defintion or NULL if parsing fails.
 */
static Ast *parse_func_definition(bool public)
{
    Ast *ast = new_ast_with_span(AST_FUNC_DEFINTION, public ? &prev_tok : &tok);

    ast->func_definition.is_public = public;
    ast->func_definition.is_exported = false;

    Ast *decl = parse_func_decl();
    if (!decl) return NULL;

    ast->func_definition.func_decl = decl;

    if (current_parser->is_interface)
	{
    	CONSUME_EOS_OR_EXIT;
    	ast->func_definition.body = NULL;
		UPDATE_AND_RETURN_AST(ast);
	}
    Ast *body = parse_compound_stmt();
    if (!body) return NULL;
    ast->func_definition.body = body;

    UPDATE_AND_RETURN_AST(ast);
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
static Ast *parse_var_definition(bool public)
{
    Ast *ast = new_ast_with_span(AST_VAR_DEFINITION, public ? &prev_tok : &tok);

    ast->var_definition.is_public = public;
    ast->var_definition.is_exported = false;

    Ast *type = parse_type(true);
    if (!type) return NULL;

    ast->var_definition.type = type;

    if (!consume(TOKEN_IDENTIFIER, "Expected variable name")) return NULL;
    ast->var_definition.name = prev_tok;

    ast->var_definition.attributes = NULL;
    if (has_attributes())
    {
        Ast *attributes = parse_attributes();
        if (!attributes) return NULL;
        ast->var_definition.attributes = attributes;
    }

    if (try_consume(TOKEN_EQ))
    {
        Ast *value = parse_init_value_and_optional_eos();
        if (!value) return NULL;
        ast->var_definition.value = value;
    }
    else
    {
        ast->var_definition.value = NULL;
        CONSUME_EOS_OR_EXIT;
    }

    UPDATE_AND_RETURN_AST(ast);

}


/**
 * array_entry ::= IDENTIFIER '+=' init_value
 */
Ast *parse_array_entry()
{
	advance_and_verify(TOKEN_IDENTIFIER);

	Ast *ast = new_ast_with_span(AST_INCREMENTAL_ARRAY, &prev_tok);
    ast->incremental_array.name = prev_tok;

	advance_and_verify(TOKEN_PLUS_ASSIGN);

	Ast *result = parse_init_value();

	if (!result) return NULL;

    ast->incremental_array.value = result;

	if (result->type != AST_STRUCT_INIT_VALUES_EXPR)
	{
	    CONSUME_EOS_OR_EXIT;
	}

    UPDATE_AND_RETURN_AST(ast);
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
			Ast *entry = parse_array_entry();
            if (entry) vector_add(current_parser->variables, entry);
			continue;
		}
		bool public = try_consume(TOKEN_PUBLIC);
		Ast *ast;
		switch (tok.type)
		{
			case TOKEN_TYPE:
				ast = parse_type_definition(public);
				if (ast) vector_add(current_parser->types, ast);
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
                ast = parse_func_definition(public);
                if (ast) vector_add(current_parser->functions, ast);
				break;
			default:
                ast = parse_var_definition(public);
                if (ast) vector_add(current_parser->variables, ast);
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
static bool is_type_expr_after_star(Ast *left_side)
{
	if (left_side->type == AST_TYPE_EXPR) return true;
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
static Ast *parse_binary(Ast *left_side)
{

	// Remember the operator.
	token_type operator_type = prev_tok.type;

	// There is a possibility that we actually have a type here.
	if (operator_type == TOKEN_STAR && is_type_expr_after_star(left_side))
	{
		return create_pointer_type(left_side);
	}
	ParseRule *rule = get_rule(operator_type);
	Ast *right_side = parse_precedence((precedence)(rule->precedence + 1));

	Ast *binary = new_ast_with_span(AST_BINARY_EXPR, &left_side->span);

	binary->binary_expr.left = left_side;
	binary->binary_expr.right = right_side;
	binary->binary_expr.operator = operator_type;

	UPDATE_AND_RETURN_AST(binary);

}

static Ast *parse_ternary(Ast *left_side)
{

	Ast *ast_ternary = new_ast_with_span(AST_TERNARY_EXPR, &left_side->span);
	ast_ternary->ternary_expr.expr = left_side;

	// Check for elvis
	if (tok.type == TOKEN_COLON)
	{
		ast_ternary->ternary_expr.true_expr = NULL;
	}
	else
	{
		Ast *true_expr = parse_precedence(PREC_TERNARY + 1);
		if (!true_expr) return NULL;
		ast_ternary->ternary_expr.true_expr = true_expr;
	}

	if (!consume(TOKEN_COLON, "Expected ':'")) return NULL;

	Ast *false_expr = parse_precedence(PREC_TERNARY + 1);

	if (!false_expr) return NULL;

	ast_ternary->ternary_expr.false_expr = false_expr;
	UPDATE_AND_RETURN_AST(ast_ternary);

}



// Right associative
static Ast *parse_binary_right(Ast *left_side)
{
	// Remember the operator.
	token_type operator_type = prev_tok.type;

	ParseRule *rule = get_rule(operator_type);
	Ast *right_side = parse_precedence((precedence)(rule->precedence));

	Ast *binary = new_ast_with_span(AST_BINARY_EXPR, &left_side->span);
	binary->binary_expr.left = left_side;
	binary->binary_expr.right = right_side;
	binary->binary_expr.operator = operator_type;
	UPDATE_AND_RETURN_AST(binary);

}

static Ast *parse_unary(Ast *left)
{
	assert(!left && "Did not expect a left hand side!");

	Ast *unary = new_ast_with_span(AST_UNARY_EXPR, &prev_tok);
	unary->unary_expr.operator = prev_tok.type;
	unary->unary_expr.expr = parse_precedence(PREC_UNARY);
	UPDATE_AND_RETURN_AST(unary);
}

static Ast *parse_post_unary(Ast *left)
{
	Ast *unary = new_ast_with_span(AST_POST_EXPR, &left->span);
	unary->post_expr.operator = prev_tok.type;
	unary->post_expr.expr = left;
	UPDATE_AND_RETURN_AST(unary);
}

static Ast *parse_grouping(Ast *left)
{
	assert(!left && "Unexpected left hand side");
	Ast *ast = parse_expression();
	consume(TOKEN_RPAREN, "Expected ')' after expression.");
	return ast;
}

static Ast *parse_call(Ast *left)
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
			Ast *param = parse_expression();
			if (!param) return NULL;
			vector_add(vector, param);
		} while (try_consume(TOKEN_COMMA));
		if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	}
	Ast *call = new_ast_with_span(AST_CALL_EXPR, &left->span);
	call->call_expr.function = left;
	call->call_expr.parameters = vector;
	UPDATE_AND_RETURN_AST(call);
}

static Ast *parse_sizeof(Ast *left)
{
	assert(!left && "Did not expect a left hand side!");

	Ast *ast = new_ast_with_span(AST_SIZEOF_EXPR, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	ast->sizeof_expr.expr = parse_expression();
	if (!ast->sizeof_expr.expr) return NULL;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;

	UPDATE_AND_RETURN_AST(ast);
}

/**
 * cast ::= CAST '(' expression ',' type_expr ')'
 *
 * @param left
 * @return
 */
static Ast *parse_cast(Ast *left)
{
	assert(!left && "Did not expect a left hand side!");

	Ast *ast = new_ast_with_span(AST_CAST_EXPR, &prev_tok);

	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	ast->cast_expr.expr = parse_expression();
	if (!ast->cast_expr.expr) return NULL;

	if (!consume(TOKEN_COMMA, "Expected ','")) return NULL;

	ast->cast_expr.type = parse_type(true);

	if (!ast->cast_expr.type) return NULL;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;

	UPDATE_AND_RETURN_AST(ast);
}

static Ast *parse_subscript(Ast *left)
{
	if (!left)
	{
		return NULL;
	}
	// Check for possible array type, if so then this is a type expression.
	if (try_consume(TOKEN_RBRACKET))
	{
		// See parse_array_type
		Ast *array = new_type_expr(TYPE_EXPR_ARRAY, &left->span);
		array->type_expr.array_type_expr.type = left;
		array->type_expr.array_type_expr.size = NULL;
		UPDATE_AND_RETURN_AST(array);
	}
	Ast *index = parse_expression();
	consume(TOKEN_RBRACKET, "Expected ]");
	if (!index) return NULL;
    Ast *subscript_ast = new_ast_with_span(AST_SUBSCRIPT_EXPR, &left->span);
	subscript_ast->subscript_expr.expr = left;
	subscript_ast->subscript_expr.index = index;
    UPDATE_AND_RETURN_AST(subscript_ast);
}


static Ast *parse_access(Ast *left)
{
	if (!left) return NULL;
	Ast *accessed = parse_expression();
	Ast *access_ast = new_ast_with_span(AST_ACCESS_EXPR, &left->span);
	access_ast->access_expr.parent = left;
	access_ast->access_expr.sub_element = accessed;
	UPDATE_AND_RETURN_AST(access_ast);
}

static void set_parse_rule(token_type type, ParseFn prefix, ParseFn infix, precedence rule_precedence)
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
	set_parse_rule(TOKEN_LESS, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS_EQ, NULL, parse_binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LEFT_SHIFT, NULL, parse_binary, PREC_SHIFT);
	set_parse_rule(TOKEN_RIGHT_SHIFT, NULL, parse_binary, PREC_SHIFT);
	set_parse_rule(TOKEN_RIGHT_SHIFT_LOGIC, NULL, parse_binary, PREC_SHIFT);
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
	set_parse_rule(TOKEN_RIGHT_SHIFT_LOGIC_ASSIGN, NULL, parse_binary, PREC_ASSIGNMENT);
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

