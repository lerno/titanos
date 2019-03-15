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
#include "header_parsing.h"
#include "semantic_analyser.h"

Parser *current_parser;

static void set_lexer(SourceRange *range)
{
	prime_lexer(range);
	advance();
}
static inline Decl *new_var_decl(SourceRange start, const char *name, VarDeclKind kind, QualifiedType type, bool public, TypeQualifier qualifiers)
{
	Decl *declaration = decl_new(DECL_VAR, start, name, false);
	//declaration->type = type;
	declaration->var.kind = kind;
	declaration->is_public = public;
	declaration->var.original_type = type;
	declaration->type = type;
	declaration->type.qualifier |= qualifiers;
	return declaration;
}



static inline bool has_attributes()
{
    return tok.type == TOKEN_AT;
}

static Ast *stmt_to_compound(Ast *inner_ast)
{
	if (!inner_ast || inner_ast->ast_id == AST_COMPOUND_STMT) return inner_ast;
	Ast *ast = new_ast_with_span(AST_COMPOUND_STMT, inner_ast->span);
	Vector *stmts = new_vector(1);
	vector_add(stmts, inner_ast);
	ast->compound_stmt.stmts = stmts;
	return ast;
}

static inline bool current_is(TokenType type)
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


static Ast *parse_stmt(void);
static Expr *parse_init_value(void);
static Ast *parse_condition(void);


void error_at_current(const char *message)
{
	error_at(tok.span, message);
}


void error_at_prev(const char *message)
{
	error_at(prev_tok.span, message);
}


bool try_consume(TokenType type)
{
    if (tok.type == type)
    {
        advance();
        return true;
    }
    return false;
}

bool consume(TokenType type, const char *message, ...)
{
    if (try_consume(type))
    {
        return true;
    }

    va_list args;
    va_start(args, message);
    verror_at(tok.span, message, args);
    va_end(args);
    return false;
}

static inline void advance_and_verify(TokenType type)
{
	assert(current_is(type) && "Not case");
	advance();
}


void recover_to(TokenType type)
{
	if (!in_panic_mode()) return;
	while (tok.type != type)
	{
		advance();
		if (tok.type == TOKEN_EOF) return;
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

	Decl *decl = decl_new(DECL_IMPORT, prev_tok.span, tok.string, false);
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
		if (prev_tok.string == current_parser->current_module)
		{
			error_at(prev_tok.span, "Cannot set alias to the current module");
			return NULL;
		}
		if (!is_lower(prev_tok.string))
		{
			error_at(prev_tok.span, "Alias must start with a lower case letter");
			return NULL;
		}
		decl->import.type = IMPORT_TYPE_ALIAS;
		decl->import.alias = prev_tok.string;
	}
	else if (try_consume(TOKEN_LOCAL))
	{
		decl->import.type = IMPORT_TYPE_LOCAL;
	}
	CONSUME_EOS_OR_EXIT;

	if (decl->name == current_parser->current_module)
   	{
   		error_at(prev_tok.span, "Cannot import the current module");
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
	Decl *import = decl_new(DECL_IMPORT, prev_tok.span, tok.string, false);
	import->is_used = true;
	import->import.type = IMPORT_TYPE_LOCAL;
	if (!consume(TOKEN_IDENTIFIER, "Expected module name")) return NULL;
	if (prev_tok.span.length > 1 && prev_tok.string[0] == '_' && prev_tok.string[1] == '_')
	{
		error_at(prev_tok.span, "Invalid module name '%s'", prev_tok.string);
	}
	if (!is_lower(prev_tok.string))
	{
		error_at(prev_tok.span, "Module must start with lower case");
	}
	if (strcmp(prev_tok.string, "c2") == 0)
	{
		error_at(prev_tok.span, "'c2' module name is reserved");
	}
	if (strcmp(prev_tok.string, "main") == 0)
	{
		error_at(prev_tok.span, "'main' module name is reserved");
	}
	current_parser->current_module = prev_tok.string;
	CONSUME_EOS_OR_EXIT;
	vector_add(current_parser->imports, import);

	UPDATE_AND_RETURN(import);
}


static Expr *parse_string_literal(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_string = expr_new(EXPR_CONST, prev_tok.span);

	char *str = malloc_arena(prev_tok.span.length + 1);
	size_t len = prev_tok.span.length;
	File *file = source_get_file(prev_tok.span.loc);

	const char *start = file->contents - file->start.id;
	memcpy(str, start + prev_tok.span.loc.id, prev_tok.span.length);

	// Just keep chaining if there are multiple parts.

	while (try_consume(TOKEN_STRING))
	{
		char *new_string = malloc_arena(len + prev_tok.span.length + 1);
		memcpy(new_string, str, len);
		memcpy(new_string + len, prev_tok.start, prev_tok.span.length);
		str = new_string;
		len += prev_tok.span.length;
	}
	str[len] = '\0';
	expr_string->const_expr.value = value_new_string(str, (uint32_t)len);
	expr_string->type = type_string();
	UPDATE_AND_RETURN(expr_string);
}

static Expr *parse_integer(Expr *left)
{
	assert(!left && "Had left hand side");
	File *file = source_get_file(prev_tok.span.loc);
	const char *start = file->contents - file->start.id + prev_tok.span.loc.id;
	Value value = parse_int(start, prev_tok.span.length);
	return sema_expr_const_int(prev_tok.span, value);
}


static Expr *parse_double(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = expr_new(EXPR_CONST, prev_tok.span);
	char *end = NULL;
	File *file = source_get_file(prev_tok.span.loc);
	const char *start = file->contents - file->start.id + prev_tok.span.loc.id;
	// IMPROVE
	long double fval = strtold(start, &end);
	if (end != prev_tok.span.length + prev_tok.start)
	{
		error_at(prev_tok.span, "Invalid float");
		return NULL;
	}
	number->const_expr.value = value_new_float(fval);
	number->type = type_compfloat();
	return number;
}

static Expr *parse_bool(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = expr_new(EXPR_CONST, prev_tok.span);
	number->const_expr.value = value_new_bool(prev_tok.type == TOKEN_TRUE);
	number->type = type_builtin_bool();
	return number;
}

static Expr *parse_nil(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *value = expr_new(EXPR_CONST, prev_tok.span);
	value->const_expr.value = value_nil();
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
        Ast *attribute = new_ast_with_span(AST_ATTRIBUTE, prev_tok.span);
        attribute->attribute.name = prev_tok.string;
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

static inline ParseRule *get_rule(TokenType type)
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

	return sema_create_identifier_expr(prev_tok);
}

/**
 * full_identifier ::= IDENTIFIER
 *                     IDENTIFIER '.' IDENTIFIER
 * @return the resulting identifier or NULL if an error occurs.
 */
Expr *parse_full_identifier()
{
	if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
	Expr *expr = expr_new(EXPR_IDENTIFIER, prev_tok.span);

	expr->identifier_expr.identifier = prev_tok.string;

	if (try_consume(TOKEN_DOT))
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected identifier after ','")) return NULL;
		Expr *sub = expr_new(EXPR_IDENTIFIER, prev_tok.span);
		sub->identifier_expr.identifier = prev_tok.string;
		Expr *result = expr_new(EXPR_ACCESS, expr->span);
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
        error_at_prev("Expected expression.");
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


TypeQualifier read_optional_type_qualifiers()
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

static void add_type_specifier(TypeOld *type, TypeQualifier type_specifier)
{
	if (!type_specifier) return;
	type->qualifier |= type_specifier;
}

/**
 * array_type ::= '[' expression ']'
 *              | '[' ']'
 * @param base the base type
 * @return the resulting type or NULL if parsing failed.
 */
static Type *parse_array_type(QualifiedType base)
{
	assert(!IS_INVALID(base));
	advance_and_verify(TOKEN_LBRACKET);

	if (try_consume(TOKEN_RBRACKET))
	{
		Type *type = type_new(TYPE_ARRAY);
		type->base = base;
		type->len = NO_LEN;
		return type;
	}
	Expr *expression = parse_expression();
	if (!expression) return NULL;
	if (!consume(TOKEN_RBRACKET, "Missing ']'")) return NULL;
	// Check const expr
	if (expression->expr_id != EXPR_CONST)
	{
		sema_error_at(expression->span, "Expected a constant expression");
		return NULL;
	}
	// Todo check len
	uint32_t len = (uint32_t)expression->const_expr.value.big_int.digit;
	Type *type = type_new(TYPE_ARRAY);
	type->base = base;
	type->len = len;
	return type;
}

#ifdef OLD
/**
 * type_specifier ::= full_identifier const?
 *                  | type_qualifier array_specifier const?
 *                  | type_qualifier '*' const?
 *                  |
 * @param accept_qualifiers read optional type specifiers (volatile, const etc)
 * @param public is this public
 * @return type
 */
TypeOld *parse_type2(bool accept_qualifiers, bool public)
{
	// IMPROVE AST span.
	TypeQualifier type_qualifier = accept_qualifiers ? read_optional_type_qualifiers() : TYPE_QUALIFIER_NONE;
	TypeOld *base;
	if (try_consume(TOKEN_VOID))
	{
		base = void_type();
		if (type_qualifier)
		{
			error_at(prev_tok.span, "Can't add type specifiers to void");
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
		type_qualifier = read_optional_type_qualifiers();
		switch (tok.type)
		{
			case TOKEN_LBRACKET:
				base = parse_array_type(base);
				break;
			case TOKEN_STAR:
				advance();
				base = type_new_pointer(base);
				break;
			default:
				if (type_qualifier)
				{
					error_at(first_specifier.span, "Unexpected type specifier here");
				}
				return base;
		}
		add_type_specifier(base, type_qualifier);
	}
}
#endif

Expr *parse_expression(void)
{
	advance();
	return parse_precedence_after_advance(PREC_ASSIGNMENT);
}

Expr *parse_expression_block(SourceRange *range)
{
    push_lexer(range);
    Expr *expr = parse_expression();
    if (!expr)
	{
    	pop_lexer();
		return NULL;
	}
    if (!expect_eof(pop_lexer(), "Invalid expession")) return NULL;
    return expr;
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
 * @param span the initial range of the parse
 * @return the created Ast* or NULL in case of error
 */
static Decl *parse_enum_type(bool public, SourceRange initial_span)
{
	CONSUME_START_BRACE_OR_EXIT;



}

/**
 * enum_body   ::= '{' '+'[opt] enum_block '}'
 *
 * enum_block  ::= enum_member
 *               | enum_block ','
 *               | enum_block ',' enum_member
 *
 * enum_member ::= IDENTIFIER
 *               | IDENTIFIER '=' expression
 *
 * @param decl the parent decl
 * @return the created Ast* or NULL in case of error
 */
bool parse_enum_body(Decl *decl)
{
	advance_and_verify(TOKEN_LBRACE);

	// TODO
	bool incremental = try_consume(TOKEN_PLUS);

	Vector *entries = new_vector(8);
	decl->enum_decl.constants = entries;

	assert(type_is_int(decl->enum_decl.type.type));
	Value value = value_new_int_with_int(0);
	value.int_bits = (uint16_t)type_bits(decl->enum_decl.type.type);

	while (tok.type != TOKEN_RBRACE)
	{
		if (!consume(TOKEN_IDENTIFIER, "Expected enum name")) return false;
		Decl *enum_constant = decl_new2(DECL_ENUM_CONSTANT, prev_tok.span.loc, &prev_tok, decl->is_public);
		vector_add(entries, enum_constant);
		enum_constant->type = decl->type;
		if (try_consume(TOKEN_EQ))
		{
			Expr *expr = parse_expression();
			if (!expr) return false;
			// Check const
			enum_constant->enum_constant.value = expr->const_expr.value;
		}
		else
		{
			// TODO prevent overflow?
			enum_constant->enum_constant.value = value;
		}
		value = value_add(value, value_new_int_with_int(1));
		if (!try_consume(TOKEN_COMMA))
		{
			if (!consume(TOKEN_RBRACE, "Expected '}'")) return false;
			break;
		}
	}
	return true;
}

QualifiedType parse_type_in_range(SourceRange *range, bool public)
{
	push_lexer(range);
	QualifiedType type = parse_type(public);
    if (IS_INVALID(type))
	{
		pop_lexer();
		return InvalidType;
	}
	if (!expect_eof(pop_lexer(), "Illegal type")) return InvalidType;
    return type;
}

Token parse_possible_struct_prefix(void)
{
	if (!try_consume(TOKEN_IDENTIFIER)) return (Token){ .type = TOKEN_VOID };
	if (tok.type != TOKEN_DOT)
	{
		sema_error_at(prev_tok.span, "Expected <identifier>.<name>");
		return (Token) { .type = TOKEN_ERROR };
	}
	Token identifier = prev_tok;
	advance_and_verify(TOKEN_DOT);
	return identifier;
}

QualifiedType type_for_token(Token *token)
{
    switch (tok.type)
    {
        case TOKEN_BOOL:
            return type_builtin_bool();
        case TOKEN_U8:
            return type_builtin_u8();
        case TOKEN_I8:
            return type_builtin_i8();
        case TOKEN_U16:
            return type_builtin_u16();
        case TOKEN_I16:
            return type_builtin_i16();
        case TOKEN_F32:
            return type_builtin_f32();
        case TOKEN_U32:
            return type_builtin_u32();
        case TOKEN_I32:
            return type_builtin_i32();
        case TOKEN_F64:
            return type_builtin_f64();
        case TOKEN_U64:
            return type_builtin_u64();
        case TOKEN_I64:
            return type_builtin_i64();
        case TOKEN_F16:
        case TOKEN_F128:
        case TOKEN_U128:
        case TOKEN_I128:
        case TOKEN_F256:
        case TOKEN_U256:
        case TOKEN_I256:
            sema_error_at(tok.span, "Unsupported type");
            return InvalidType;
        default:
            sema_error_at(tok.span, "Expected type name");
            return InvalidType;
    }
}


QualifiedType parse_type(bool public)
{
	QualifiedType base = InvalidType;
	TypeQualifier qualifier = read_optional_type_qualifiers();
	if (try_consume(TOKEN_VOID))
	{
		qualifier |= read_optional_type_qualifiers();
		if (qualifier != TYPE_QUALIFIER_NONE)
		{
			error_at(prev_tok.span, "Can't add type specifiers to void");
			return InvalidType;
		}
		base = void_type();
	}
	else
	{
		// If it's not an identifier it must be a built in type.
		if (!try_consume(TOKEN_IDENTIFIER))
		{
			base = type_for_token(&tok);
			advance();
		}
		else
		{
			if (tok.type == TOKEN_DOT)
			{
				// Lookup import
				Token module = prev_tok;
				advance();
				if (!consume(TOKEN_IDENTIFIER, "Expected type name")) return InvalidType;
				base = sema_resolve_prefixed_type(module, prev_tok);
			}
			else
			{
				base = sema_resolve_type(prev_tok, public);
			}
		}
		if (IS_INVALID(base)) return base;

		qualifier |= read_optional_type_qualifiers();
		if (qualifier)
		{
			base.qualifier = qualifier;
		}
	}
	while (true)
	{
		switch (tok.type)
		{
			case TOKEN_LBRACKET:
				base = (QualifiedType) { .type = parse_array_type(base) };
				break;
			case TOKEN_STAR:
				advance();
				base = type_new_pointer(base);
				break;
			default:
				qualifier = read_optional_type_qualifiers();
				if (qualifier)
				{
					base.qualifier = qualifier;
				}
				return base;
		}
		if (IS_INVALID(base)) return base;
	}
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

	TypeOld *type = NULL;// TODO parse_type2(true, public);
	if (!type) return NULL;
#if 0
    Decl *param_decl = new_var_decl(start.span, start.string, VARDECL_PARAM, type, public, type->qualifier);
	if (!try_consume(TOKEN_IDENTIFIER))
	{
		param_decl->name = NULL;
	}
	else
	{
		param_decl->name = prev_tok.string;
	}

	if (try_consume(TOKEN_EQEQ))
	{
		if (!param_decl->name)
		{
			error_at(prev_tok.span, "Expected variable name with default value");
			return NULL;
		}
		param_decl->var.init_expr = parse_expression();
		if (!param_decl->var.init_expr) return NULL;
	}

	if (has_default_args && !param_decl->var.init_expr)
	{
		error_at(param_decl->span, "Parameters without default value must preceed the ones with default");
	}
	UPDATE_AND_RETURN(param_decl);
#endif
}

/**
 * param_list ::= '...'
 *                | param_decl
 *                | param_list ',' param_list
 *                ;
 * @return ast or NULL
 */
bool parse_argument_list(Decl *decl, bool public, bool type_only)
{
	if (!consume(TOKEN_LPAREN, "Expected '('")) return false;
	if (try_consume(TOKEN_RPAREN))
	{
		decl->func_decl.args = new_vector(0);
	    decl->func_decl.variadic = false;
		return true;
	}
	Vector *args = new_vector(6);
	decl->func_decl.variadic = false;
	bool had_defaults = false;
	while (true)
	{
		if (try_consume(TOKEN_ELIPSIS))
		{
            decl->func_decl.variadic = true;
		}
		else
		{
			Decl *param_decl = parse_param_decl(public, had_defaults);
			if (!param_decl) return false;
			if (param_decl->var.init_expr)
			{
				had_defaults = true;
				if (type_only)
				{
					error_at(decl->var.init_expr->span, "Default arguments are not allowed here");
					had_defaults = false;
				}
			}
			vector_add(args, decl);
		}
		if (!try_consume(TOKEN_COMMA)) break;
		if (decl->func_decl.variadic)
		{
			error_at_current("Variadic parameter followed by arguments");
			return false;
		}
	}
	decl->func_decl.args = args;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return true;

	return true;
}

/**
 * macro_argument_list ::= '(' macro_arguments ')'
 *                       | '(' macro_arguments ',' '...' ')'
 *                       | '(' '...' ')'
 *
 * macro_arguments     ::= macro_argument (',' macro_argument)*
 *
 * macro_argument      ::= '&' IDENTIFIER
 *                       | IDENTIFIER
 *
 * @param macro_decl
 * @return true if parsing succeeded
 */
static inline bool parse_macro_argument_list(MacroDecl *macro_decl)
{
    if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

    if (try_consume(TOKEN_RPAREN))
    {
        macro_decl->args = new_vector(0);
        macro_decl->variadic = false;
        return true;
    }
    Vector *args = new_vector(6);
    macro_decl->variadic = false;
    bool had_defaults = false;
    while (true)
    {
        if (try_consume(TOKEN_ELIPSIS))
        {
        	if (had_defaults)
			{
        		sema_error_at(prev_tok.span, "Variadic parameters not accepted after default params");
			}
            macro_decl->variadic = true;
        }
        else
        {
            bool is_ref = try_consume(TOKEN_AMP);
            SourceRange start = prev_tok.span;
            if (!consume(TOKEN_IDENTIFIER, "Expected a parameter name")) return false;

            Decl *decl = decl_new(DECL_MACRO_PARAM, start, prev_tok.string, false);
            decl->macro_param.is_ref = is_ref;
            UPDATE(decl);
            if (try_consume(TOKEN_EQ))
			{
            	Expr *expr = parse_expression();
            	if (!expr) return false;
            	decl->macro_param.init_expr = expr;
            	had_defaults = true;
			}
            else
			{
            	if (had_defaults)
				{
            		sema_error_at(decl->span, "Non-default argument after default arg");
            		had_defaults = false;
				}
			}
            if (decl->var.init_expr)
            {
                had_defaults = true;
            }
            vector_add(args, decl);
        }
        if (!try_consume(TOKEN_COMMA)) break;
        if (macro_decl->variadic)
        {
            error_at_current("Variadic parameter followed by arguments");
            return false;
        }
    }
    macro_decl->args = args;
    if (!consume(TOKEN_RPAREN, "Expected ')'")) return false;
    return true;
}
/**
 * macro_decl ::= IDENTIFIER macro_argument_list
 * @param public is a public macro
 * @return Decl* or NULL if parsing fails.
 */
static inline Decl *parse_macro_decl(bool public)
{
    Token start = tok;
    advance_and_verify(TOKEN_MACRO);

    if (!consume(TOKEN_IDENTIFIER, "Expected macr name but got '%s'", tok.string)) return NULL;

    Decl *decl = decl_new(DECL_MACRO, start.span, prev_tok.string, public);

    if (!parse_macro_argument_list(&decl->macro_decl)) return NULL;

    UPDATE_AND_RETURN(decl);
}

static Ast *parse_compound_stmt()
{
    CONSUME_START_BRACE_OR_EXIT;
	Ast *compound = new_ast_with_span(AST_COMPOUND_STMT, prev_tok.span);
	Vector *stmts = new_vector(0);
	compound->compound_stmt.stmts = stmts;
	while (1)
	{
		if (try_consume(TOKEN_RBRACE))
		{
			range_expand(&compound->span, &prev_tok);
			return compound;
		}
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
	Ast *if_ast = new_ast_with_span(AST_IF_STMT, prev_tok.span);

	if (!consume(TOKEN_LPAREN, "Expected (")) return NULL;

	Ast *cond = parse_condition();
	if (!cond) return NULL;

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
	if_ast->if_stmt.cond = cond;
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
	Ast *while_ast = new_ast_with_span(AST_WHILE_STMT, prev_tok.span);
	if (!consume(TOKEN_LPAREN, "Expected (")) return NULL;
	Ast *cond = parse_condition();
	if (!cond) return NULL;
	if (!consume(TOKEN_RPAREN, "Expected )")) return NULL;
	Ast *body = parse_stmt();
	if (!body) return NULL;
	while_ast->while_stmt.body = stmt_to_compound(body);
	while_ast->while_stmt.cond = cond;
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
	Ast *do_ast = new_ast_with_span(AST_DO_STMT, prev_tok.span);

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
	Ast *defer_stmt = new_ast_with_span(AST_DEFAULT_STMT, prev_tok.span);
	Ast *defer_body = parse_stmt();
	if (!defer_body) return NULL;
	defer_stmt->defer_stmt.body = stmt_to_compound(defer_body);
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
	Ast *ast = new_ast_with_span(AST_CASE_STMT, prev_tok.span);
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
	Ast *ast = new_ast_with_span(AST_DEFAULT_STMT, prev_tok.span);
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

	Ast *ast = new_ast_with_span(AST_SWITCH_STMT, prev_tok.span);
	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	Ast *cond = parse_condition();
	if (!cond) return NULL;

	if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;


	ast->switch_stmt.cond = cond;

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
					error_at(case_stmt->span, "Default defined twice");
					prev_at(ast->switch_stmt.default_stmt->span, "Previous definition");
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
	Ast *ast = new_ast_with_span(AST_BREAK_STMT, prev_tok.span);
	CONSUME_REQ_EOS_AND_RETURN(ast);
}

/**
 * return_stmt ::= RETURN expression EOS
 * @return Ast* if parsing succeeded, NULL otherwise
 */
static Ast *parse_return_stmt()
{
	advance_and_verify(TOKEN_RETURN);
	Ast *ast = new_ast_with_span(AST_RETURN_STMT, prev_tok.span);
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
	Ast *ast = new_ast_with_span(AST_GOTO_STMT, prev_tok.span);

	if (!consume(TOKEN_IDENTIFIER, "Expected a label as destination for goto")) return false;

	ast->goto_stmt.label_name = prev_tok.string;
	ast->goto_stmt.type = GOTO_NOT_ANALYSED;

	CONSUME_REQ_EOS_AND_RETURN(ast);
}

/**
 * continue_stmt ::= CONTINUE EOS
 * @return Ast *
 */
static Ast *parse_continue_stmt()
{
	advance_and_verify(TOKEN_CONTINUE);
	Ast *ast = new_ast_with_span(AST_CONTINUE_STMT, prev_tok.span);
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
	Ast *stmt = new_ast_with_span(AST_EXPR_STMT, expression->span);
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
	Expr *expr = expr_new(EXPR_DESIGNATED_INITIALIZER, prev_tok.span);

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

	Expr *expr = expr_new(EXPR_STRUCT_INIT_VALUES, tok.span);
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

	TypeQualifier type_qualifiers = read_optional_type_qualifiers();

	TypeOld *type_expr = NULL; // TODO parse_type2(false, false);
	if (!type_expr) return NULL;

#if 0
    Decl *declaration = new_var_decl(start.span, tok.string, VARDECL_LOCAL, type_expr, false, type_qualifiers);

	if (!consume(TOKEN_IDENTIFIER, "Expected variable name")) return NULL;

    declaration->name = prev_tok.string;

    Expr *init_expr = try_consume(TOKEN_EQ) ? parse_init_value() : NULL;

	declaration->var.init_expr = init_expr;

	UPDATE_AND_RETURN(declaration);
#endif
}

/**
 * label_stmt := IDENTIFIER ':'
 * @return Ast*
 */
static Ast *parse_label_stmt()
{
	advance_and_verify(TOKEN_IDENTIFIER);
	Ast *ast = new_ast_with_span(AST_LABEL, prev_tok.span);
	ast->label_stmt.label_name = prev_tok.string;
	ast->label_stmt.is_used = false;
//	ast->label_stmt.defer_top = NULL;
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
	Ast *decl_stmt = new_ast_with_span(AST_DECLARE_STMT, declaration->span);
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
	Ast *for_ast = new_ast_with_span(AST_FOR_STMT, prev_tok.span);

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
 * @param parent
 */
bool parse_struct_block(Decl *parent)
{
	if (!consume(TOKEN_LBRACE, "Expected '{' to begin definition")) return false;

	parent->struct_decl.members = new_vector(4);

	while (true)
	{
		if (tok.type == TOKEN_RBRACE) break;


		if (tok.type == TOKEN_STRUCT || tok.type == TOKEN_UNION)
		{
			Decl *member = decl_new2(tok.type == TOKEN_STRUCT ? DECL_STRUCT_TYPE : DECL_UNION_TYPE, tok.span.loc, &tok, parent->is_public);
			decl_add_own_type(member, tok.type == TOKEN_STRUCT ? TYPE_STRUCT : TYPE_UNION);
			advance();
			if (try_consume(TOKEN_IDENTIFIER))
			{
				member->name = prev_tok.string;
			}
			else
			{
				member->name = NULL;
			}
			if (!parse_struct_block(member)) return false;
			range_expand(&member->span, &prev_tok);
			vector_add(parent->struct_decl.members, member);
			continue;
		}
		TypeQualifier qualifiers = read_optional_type_qualifiers();
		QualifiedType type = parse_type(parent->is_public);
		if (IS_INVALID(type)) return false;
		Decl *member = new_var_decl(tok.span, tok.string, VARDECL_MEMBER, type, parent->is_public, qualifiers);
		if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return false;
		member->name = prev_tok.string;
		range_expand(&member->span, &prev_tok);
		if (!consume(TOKEN_EOS, "Expected ';'")) return false;
	}
	if (!consume(TOKEN_RBRACE, "Expected '}'")) return false;
	return true;

}

bool parse_struct_body(Decl *struct_decl, SourceRange body)
{
	LEXER_BEGIN(&body)
	LEXER_EVAL = parse_struct_block(struct_decl)
	LEXER_END
	return true;
}



/**
 * alias_type ::= IDENTIFIER type_specifier

 * @param public if the alias is public
 * @param span the initial span of the parse.
 * @return the created Decl* or NULL in case of error
 */
static Decl *parse_alias_type(bool public, SourceRange span)
{
	Decl *alias = decl_new(DECL_ALIAS_TYPE, span, prev_tok.string, public);
	// TODO TypeOld *type = parse_type2(true, public);
	// TODOif (!type) return NULL;
	// TODO alias->type = type;

    // TODO parse attributes?
	CONSUME_REQ_EOS_AND_RETURN(alias);
}




/**
 * macro_definition ::= macro_type compund_statement
 *
 * @param public is the macro is public
 * @return Decl *with the macro defintion or NULL if parsing fails.
 */
static Decl *parse_macro_definition(bool public)
{
	Decl *decl = parse_macro_decl(public);
	if (!decl) return NULL;

	Ast *body = parse_compound_stmt();
	if (!body) return NULL;
	decl->macro_decl.body = body;

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

	TypeQualifier qualifier = read_optional_type_qualifiers();

    TypeOld *type = NULL; // TODO parse_type2(false, public);
    if (!type) return NULL;

#if  0
    if (!consume(TOKEN_IDENTIFIER, "Expected variable name")) return NULL;

	Decl *decl = new_var_decl(start.span, prev_tok.string, VARDECL_GLOBAL, type, public, qualifier);

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
#endif
}


/**
 * array_entry ::= IDENTIFIER '+=' init_value
 */
Decl *parse_array_entry()
{
	advance_and_verify(TOKEN_IDENTIFIER);

	Decl *array = decl_new(DECL_ARRAY_VALUE, prev_tok.span, prev_tok.string, false);

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
#ifdef OLD
/**
 * top_level ::= top_level
 *             | top_level top_level_item
 *
 * top_level_item ::= array_entry                     -> IDENTIFIER += init_value
 *                  | public[opt] type_definition     -> [public] type IDENTIFIER ...
 *                  | public[opt] function            -> [public] func ...
 *                  | public[opt] var_definition      -> [public] type_qualifier IDENTIFIER
 *                  | public[opt] macro               -> [public] macro ...
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
		    case TOKEN_MACRO:
		        decl = parse_macro_definition(public);
		        if (decl) vector_add(current_parser->macros, decl);
                break;
			default:
                decl = parse_var_definition(public);
                if (decl) vector_add(current_parser->variables, decl);
				break;
		}
	}
}

#endif
/**
 * source ::= module import top_level
 * @return false if error occurs;
 */
bool parse_source()
{
	parse_module();
	parse_imports();
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
	TokenType operator_type = prev_tok.type;

	// There is a possibility that we actually have a type here.
	if (operator_type == TOKEN_STAR && is_type_expr_after_star(left_side))
	{
		TypeOld *type = new_type(XTYPE_POINTER, false);
		type->pointer.base = new_unresolved_type(left_side, false);
		Expr *expr = expr_new_type_expr(type, left_side->span);
		UPDATE_AND_RETURN(expr);
	}
	ParseRule *rule = get_rule(operator_type);
	Expr *right_side = parse_precedence(rule->precedence + 1);

	Expr *expr = sema_expr_binary(operator_type, left_side, right_side);

	UPDATE_AND_RETURN(expr);

}

static Expr *parse_ternary(Expr *left_side)
{

	Expr *expr_ternary = expr_new(EXPR_TERNARY, left_side->span);
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
	TokenType operator_type = prev_tok.type;

	ParseRule *rule = get_rule(operator_type);
	Expr *right_side = parse_precedence((Precedence)(rule->precedence));

	Expr *binary = expr_new(EXPR_BINARY, left_side->span);
	binary->binary_expr.left = left_side;
	binary->binary_expr.right = right_side;
	binary->binary_expr.operator = binop_from_token(operator_type);
	UPDATE_AND_RETURN(binary);

}

static Expr *parse_unary(Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	Expr *unary = expr_new(EXPR_UNARY, prev_tok.span);
	unary->unary_expr.operator = unaryop_from_token(prev_tok.type);
	unary->unary_expr.expr = parse_precedence(PREC_UNARY);
	UPDATE_AND_RETURN(unary);
}

static Expr *parse_post_unary(Expr *left)
{
	Expr *unary = expr_new(EXPR_POST, left->span);
	unary->post_expr.operator = unaryop_from_token(prev_tok.type);
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
	Expr *call = expr_new(EXPR_CALL, left->span);
	call->call_expr.function = left;
	call->call_expr.parameters = vector;
	UPDATE_AND_RETURN(call);
}

static Expr *parse_sizeof(Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	Expr *expr = expr_new(EXPR_SIZEOF, prev_tok.span);

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

	Expr *expr = expr_new(EXPR_CAST, prev_tok.span);

	if (!consume(TOKEN_LPAREN, "Expected '('")) return NULL;

	expr->cast_expr.expr = parse_expression();
	if (!expr->cast_expr.expr) return NULL;

	if (!consume(TOKEN_COMMA, "Expected ','")) return NULL;

	expr->type = parse_type(false);
	if (IS_INVALID(expr->type)) return NULL;

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
		TypeOld *type = new_type(XTYPE_ARRAY, false);
		type->array.base = new_unresolved_type(left, false);
		type->array.is_empty = true;
		type->array.is_len_resolved = true;
        Expr *expr = expr_new_type_expr(type, left->span);
        UPDATE_AND_RETURN(expr);
	}
	Expr *index = parse_expression();
	consume(TOKEN_RBRACKET, "Expected ]");
	if (!index) return NULL;
    Expr *subscript_ast = expr_new(EXPR_SUBSCRIPT, left->span);
	subscript_ast->subscript_expr.expr = left;
	subscript_ast->subscript_expr.index = index;
    UPDATE_AND_RETURN(subscript_ast);
}


static Expr *parse_access(Expr *left)
{
	if (!left) return NULL;
	Expr *accessed = parse_expression();
	Expr *access_expr = expr_new(EXPR_ACCESS, left->span);
	access_expr->access_expr.parent = left;
	access_expr->access_expr.sub_element = accessed;
	UPDATE_AND_RETURN(access_expr);
}

static Ast *parse_condition(void)
{
	Ast *cond_stmt;
	if (is_next_decl())
	{
		Decl* declaration = parse_declaration();
		if (!declaration) return NULL;
		cond_stmt = new_ast_with_span(AST_COND_STMT, declaration->span);
		cond_stmt->cond_stmt.decl = declaration;
		cond_stmt->cond_stmt.cond_type = COND_DECL;
	}
	else
	{
		Expr *expr = parse_expression();
		if (!expr) return NULL;
		cond_stmt = new_ast_with_span(AST_COND_STMT, expr->span);
		cond_stmt->cond_stmt.expr = expr;
		cond_stmt->cond_stmt.cond_type = COND_EXPR;
	}
	return cond_stmt;
}

static void set_parse_rule(TokenType type, ParseFn prefix, ParseFn infix, Precedence rule_precedence)
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


Parser *parse(Component *component, Module *module, const char *filename, bool is_interface)
{
    size_t size;
    char *file = read_file(filename, &size);
	init_semantic_analyser(component, module, filename, is_interface);
	init_lexer(filename, file, size);
	parse_header();
	sema_analyse();
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

