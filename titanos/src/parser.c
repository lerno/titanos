//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include <assert.h>
#include "parser.h"
#include "lexer.h"
#include "debug.h"
#include "string.h"
#include "fnv.h"
#include "expression.h"
#include <memory.h>

#define QUALIFIED_NAME_MAX 256


typedef struct
{
	Token start;
	Token end;
} QualifiedName;

typedef struct _Scope
{
} Scope;

#define MAX_SCOPES 128

typedef struct
{
	Scope scopes[MAX_SCOPES];
	char *module;
	Token current;
	Token previous;
	bool had_error;
	bool panic_mode;
	uint8_t active_scope;

} Parser;

typedef Expression *(*InfixFn)(Expression *);
typedef Expression *(*PrefixFn)(void);

typedef enum
{
	PREC_NONE,
	PREC_ASSIGNMENT,  // =
	PREC_OR,          // or
	PREC_AND,         // and
	PREC_COMPARISON,  // < > <= >= == !=
	PREC_TERM,        // + -
	PREC_BITWISE,     // ^ | &
	PREC_SHIFT,       // << >> >>>
	PREC_FACTOR,      // * / %
	PREC_POW,         // **
	PREC_UNARY,       // ! - + ~
	PREC_CALL,        // . () []
	PREC_PRIMARY
} precedence;

typedef struct
{
	PrefixFn prefix;
	InfixFn infix;
	precedence precedence;
} ParseRule;


static ParseRule rules[TOKEN_EOF + 1];

Parser parser;

void import(void);
void proc(void);

static void set_parse_rule(token_type type, PrefixFn prefix, InfixFn infix, precedence rule_precedence)
{
	rules[type].prefix = prefix;
	rules[type].precedence = rule_precedence;
	rules[type].prefix = prefix;
	rules[type].infix = infix;
};



// IDENTIFIER | proc_type -> type | void | type *
void type(void);


static void error_at(Token *token, const char *message)
{
	if (parser.panic_mode) return;
	parser.panic_mode = true;

	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF)
	{
		fprintf(stderr, " at end");
	}
	else if (token->type == TOKEN_ERROR)
	{
		// Nothing.
	}
	else
	{
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	parser.had_error = true;
}


static void error_at_current(const char *message)
{
	error_at(&parser.current, message);
}


static void error(const char *message)
{
	error_at(&parser.previous, message);
}

static void push_scope()
{
	if (parser.active_scope == MAX_SCOPES)
	{
		error_at_current("Cannot nest deeper");
		return;
	}
	parser.active_scope++;
}

static void pop_scope()
{
	assert(parser.active_scope > 0 && "Tried to pop top scope");
	parser.active_scope--;
}

static void advance(void)
{
	parser.previous = parser.current;

	while (1)
	{
		parser.current = scan_token();
		printf("%s",  token_name(&parser.current));
		if (parser.current.type != TOKEN_ERROR) break;
		error_at_current(parser.current.start);
	}
}

static void consume(token_type type, const char *message)
{
	if (parser.current.type == type)
	{
		advance();
		return;
	}
	error_at_current(message);
}

static QualifiedName qualified_name_with_wildcard()
{
	Token start = parser.current;
	consume(TOKEN_IDENTIFIER, "Expected name");
	while (parser.current.type == TOKEN_COLCOLON)
	{
		advance();
		if (parser.current.type == TOKEN_MULT)
		{
			advance();
			break;
		}
		consume(TOKEN_IDENTIFIER, "Expected name");
	}
	QualifiedName qualified_name = { .start = start, .end = parser.previous };
	return qualified_name;
}

static inline void copy_token_to_buffer(Token *token, void *buffer, int *position, int buffer_size)
{
	if (buffer_size - *position < token->length)
	{
		error_at_current("Identifier too long");
	}
	memcpy(buffer + *position, token->start, token->length);
	*position += token->length;
}

static char *qualified_name()
{
	consume(TOKEN_IDENTIFIER, "Expected name");
	char buffer[QUALIFIED_NAME_MAX];
	int position = 0;
	copy_token_to_buffer(&parser.previous, buffer, &position, QUALIFIED_NAME_MAX);
	while (parser.current.type == TOKEN_COLCOLON)
	{
		advance();
		copy_token_to_buffer(&parser.previous, buffer, &position, QUALIFIED_NAME_MAX);
		consume(TOKEN_IDENTIFIER, "Expected name");
		copy_token_to_buffer(&parser.previous, buffer, &position, QUALIFIED_NAME_MAX);
	}
	char *result = malloc(position + 1);
	memcpy(result, buffer, position);
	result[position] = 0;
	return result;
}

void recover_to(token_type type)
{
	if (!parser.panic_mode) return;
	while (parser.current.type != type)
	{
		advance();
		if (parser.current.type == TOKEN_EOF) return;
	}
	advance();
	parser.panic_mode = false;
}

// import: IMPORT qualified_name (::*)? EOS
void import()
{
	advance();
	// TODO properly handle the imports.
	qualified_name_with_wildcard();
	consume(TOKEN_EOS, "Expected ;");
	recover_to(TOKEN_EOS);
}

// module: MODULE qualified_name EOS
void module()
{
	advance();
	parser.module = qualified_name();
	consume(TOKEN_EOS, "Expected ;");
	recover_to(TOKEN_EOS);
}

void var_list()
{
	type();
	while (parser.current.type == TOKEN_COMMA)
	{
		type();
	}
}

// PROC ( type_list )
void proc_type()
{
	// Skip "proc"
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	var_list();
	consume(TOKEN_PAREN_R, "Expected )");
	consume(TOKEN_ARROW, "Expected ->");
	type();
}

void primitive_type(void)
{
	advance();
	// Consume all pointers
	while (parser.current.type == TOKEN_MULT) advance();
}

void pointer_type(void)
{
	// (
	advance();
	type();
	consume(TOKEN_PAREN_R, "Expected )");
	while (parser.current.type == TOKEN_MULT) advance();
}
void complex_type(void)
{
	switch (parser.current.type)
	{
		case TOKEN_PROC:
			proc_type();
			break;
		case TOKEN_IDENTIFIER:
			primitive_type();
			break;
		case TOKEN_PAREN_L:
			pointer_type();
			break;
		default:
			error_at_current("Expected a type");
			break;
	}

}
// IDENTIFIER | proc_type -> type | void | type *
void type()
{
	if (parser.current.type == TOKEN_VOID)
	{
		advance();
		return;
	}

	complex_type();
}
void def_struct()
{
	advance();
	consume(TOKEN_IDENTIFIER, "Expected struct name");
	consume(TOKEN_BRACE_L, "Expected {");
	while (parser.current.type == TOKEN_IDENTIFIER)
	{
		advance();
		consume(TOKEN_COLON, "Expected type");
		type();
		consume(TOKEN_EOS, "Expected ;");
		recover_to(TOKEN_EOS);
	}
	consume(TOKEN_BRACE_R, "Expected }");
}

void named_var_list()
{
	while (parser.current.type != TOKEN_PAREN_R)
	{
		consume(TOKEN_IDENTIFIER, "Expected identifier");
		consume(TOKEN_COLON, "Expected :");
		type();
		if (parser.current.type != TOKEN_PAREN_R)
		{
			consume(TOKEN_COMMA, "Expected ,");
		}
	}
}


static Expression *expression_after_advance(void);

static Expression *expression()
{
	advance();
	return expression_after_advance();
}

static Expression *return_statement()
{
	advance();
    Expression *return_expr = expr_new_of_type(RETURN_EXPRESSION);
    expr_add_param(return_expr, expression());
    return return_expr;
}

Expression *no_block_statement()
{
	switch (parser.current.type)
	{
		case TOKEN_RETURN:
			return return_statement();
		case TOKEN_IDENTIFIER:
			return variable_declaration_or_expression;
		default:
			return NULL;
	}
}

static Expression *braced_statements(void);

static Expression *block_body()
{
	if (parser.current.type == TOKEN_ARROW)
	{
		Expression *statement = no_block_statement();
		if (!statement)
		{
			error_at_current("Expected statement");
		}
		consume(TOKEN_EOS, "Expected ;");
		return statement;
	}
	return braced_statements();
}

static Expression *while_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	Expression *cond = expression();
	consume(TOKEN_PAREN_R, "Expected )");
	Expression *body = block_body();
	Expression *while_expr = expr_new_of_type(LOOP_EXPRESSION);
	expr_add_param(while_expr, cond);
	expr_add_param(while_expr, body);
	return while_expr;
}

Expression *until_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	Expression *cond = expression();
	consume(TOKEN_PAREN_R, "Expected )");
	Expression *body = block_body();
	Expression *until_expr = expr_new_of_type(ENDLOOP_EXPRESSION);
	expr_add_param(until_expr, cond);
	expr_add_param(until_expr, body);
	return until_expr;
}

static Expression *if_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	Expression *cond = expression();
	consume(TOKEN_PAREN_R, "Expected )");
	Expression *body;
	Expression *else_body = NULL;
	if (parser.current.type == TOKEN_ARROW)
	{
		body = no_block_statement();
		if (!body)
		{
			error_at_current("Expected statement");
			recover_to(TOKEN_EOS);
		}
		consume(TOKEN_EOS, "Expected ;");
	}
	else
	{
		body = braced_statements();
		if (body && parser.current.type == TOKEN_ELSE)
		{
			advance();
			else_body = braced_statements();
			if (!else_body) return NULL; // Dealloc cond + body
		}
	}

	if (!body) return NULL; // Dealloc cond
	if (cond == NULL) return NULL; // Dealloc body/else_body

	Expression *if_statement = expr_new_of_type(AND_EXPRESSION);
	expr_add_param(if_statement, body);
	if (else_body) expr_add_param(if_statement, else_body);
	return if_statement;
}

Expression *statement()
{
	switch (parser.current.type)
	{
		case TOKEN_IF:
			return if_statement();
		case TOKEN_WHILE:
			return while_statement();
		case TOKEN_UNTIL:
			return until_statement();
			/*
		case TOKEN_FOR:
			for_statement();
			break;
		case TOKEN_SWITCH:
			switch_statement();
			break;*/
		default:
			break;
	}
	Expression *expr = no_block_statement();
    if (!expr) return NULL;
    consume(TOKEN_EOS, "Expected ;");
	return expr;
}
Expression *statements()
{
	Expression *expression = expr_new_of_type(STMT_EXPRESSION);
	Expression *stmt;
	while ((stmt = statement()) != NULL)
	{
		expr_add_param(expression, stmt);
	};
	return expression;
}

static Expression *braced_statements()
{
	consume(TOKEN_BRACE_L, "Expected {");
	Expression *expression = statements();
	consume(TOKEN_BRACE_R, "Expected }");
	return expression;
}
void proc()
{
	advance();
	consume(TOKEN_IDENTIFIER, "Expected name");
	consume(TOKEN_PAREN_L, "Expected (");
	named_var_list();
	consume(TOKEN_PAREN_R, "Expected )");
	consume(TOKEN_ARROW, "Expected ->");
	type();
	Expression *proc = braced_statements();
    print_ast(proc);
}

void program()
{
	error_at_current("Unhandled");
	advance();
}

void extend_struct()
{
	advance();
	qualified_name_with_wildcard();
	consume(TOKEN_BRACE_L, "Expected {");
	while (parser.current.type != TOKEN_BRACE_R)
	{
		proc();
	}
	consume(TOKEN_BRACE_R, "Expected end");
}

// source: module? import* (proc | program)* ;
void source()
{
	push_scope();
	if (parser.current.type == TOKEN_MODULE)
	{
		module();
	}
	while (parser.current.type == TOKEN_IMPORT)
	{
		import();
	}
	while (parser.current.type != TOKEN_EOF)
	{
		switch (parser.current.type)
		{
			case TOKEN_EXTEND:
				extend_struct();
				break;
			case TOKEN_STRUCT:
				def_struct();
				break;
			case TOKEN_IMPORT:
				import();
				break;
			case TOKEN_PROC:
				proc();
				break;
			case TOKEN_PROGRAM:
				program();
				break;
			default:
				error_at_current("Expected import, proc or program");
				recover_to(TOKEN_BRACE_R);
		}
	}
	pop_scope();
}

static inline ParseRule *get_rule(token_type type)
{
	return &rules[type];
}

static Expression *parse_precedence_after_advance(precedence precedence)
{
	// Get the rule for the previous token.
	PrefixFn prefix_rule = get_rule(parser.previous.type)->prefix;
	if (prefix_rule == NULL)
	{
		error("Expected expression.");
		return NULL;
	}

	Expression *expression = prefix_rule();

	while (precedence <= get_rule(parser.current.type)->precedence)
	{
		advance();
		InfixFn infix_rule = get_rule(parser.previous.type)->infix;
		expression = infix_rule(expression);
	}
	return expression;
}

static Expression *parse_precedence(precedence precedence)
{
	advance();
	return parse_precedence_after_advance(precedence);
}

static Expression *expression_after_advance()
{
	return parse_precedence_after_advance(PREC_ASSIGNMENT);
}

static Expression *binary(Expression *left_operand)
{
	// Remember the operator.
	token_type operator_type = parser.previous.type;

	// Compile the right operand.
	ParseRule *rule = get_rule(operator_type);
	Expression *right_operand = parse_precedence((precedence)(rule->precedence + 1));
	// Right hand side now loaded.

	Expression *expression = expr_new_of_type(BINARY_EXPRESSION);
	expression->data.token_type = operator_type;
	expr_add_param(expression, left_operand);
	expr_add_param(expression, right_operand);
	return expression;
}


// Right associative
static Expression *binary_right(Expression *left_operand)
{
	// Remember the operator.
	token_type operator_type = parser.previous.type;

	// Compile the right operand.
	ParseRule *rule = get_rule(operator_type);
	Expression *right_operand = parse_precedence((precedence)(rule->precedence));
	// Right hand side now loaded.

	Expression *expression = expr_new_of_type(BINARY_EXPRESSION);
	expression->data.token_type = operator_type;
	expr_add_param(expression, left_operand);
	expr_add_param(expression, right_operand);
	return expression;
}


static Expression *unary()
{
	token_type operatorType = parser.previous.type;

	// Compile the operand.
	Expression *right_operand = parse_precedence(PREC_UNARY);

	Expression *expression = expr_new_of_type(UNARY_EXPRESSION);
	expression->data.token_type = operatorType;
	expr_add_param(expression, right_operand);
	return expression;
}

static Expression *identifier()
{
	Expression *expr = expr_new_of_type(IDENT_EXPRESSION);
	// DO_STUFF
	return NULL;
}

static Expression *grouping()
{
	Expression *expr = expression();
	consume(TOKEN_PAREN_R, "Expected ')' after expression.");
	return expr;
}

static Expression *call(Expression *expr)
{
	Expression *call_expr = expr_new_of_type(CALL_EXPRESSION);
	expr_add_param(call_expr, expr);
    if (parser.current.type != TOKEN_PAREN_R)
    {
        do
        {
	        expr_add_param(call_expr, expression());
        } while (parser.current.type == TOKEN_COMMA);
    }
    consume(TOKEN_PAREN_R, "Expected )");
	return call_expr;
}

static Expression *access(Expression *expression)
{
	// TODO BROOOKEN
    consume(TOKEN_IDENTIFIER, "Expected identifier");
    // Three possibilities: () [] .
    switch (parser.current.type)
    {
        case TOKEN_PAREN_L:
	        return call(expression);
        case TOKEN_BRACKET_L: // TODO
	        return NULL;
        case TOKEN_DOT:
	        return access(expression);
        default:
	        return parse_precedence_after_advance(PREC_CALL);
    }
}

static Expression *or(Expression *left_hand)
{
	Expression *right_hand = parse_precedence(PREC_OR);
	Expression *or_expr = expr_new_of_type(OR_EXPRESSION);
	expr_add_param(or_expr, left_hand);
	expr_add_param(or_expr, right_hand);
	return or_expr;
}

static Expression *and(Expression *left_hand)
{
	Expression *right_hand = parse_precedence(PREC_AND);
	Expression *and_expr = expr_new_of_type(AND_EXPRESSION);
	expr_add_param(and_expr, left_hand);
	expr_add_param(and_expr, right_hand);
	return and_expr;
}


static Expression *integer_number(void)
{
	Expression *expr = expr_new_of_type(INTEGER_EXPRESSION);
	uint64_t number = parse_uint64(parser.previous.start, parser.previous.length);
	expr->data.numvalue = number;
	return expr;
}


static Expression *double_number(void)
{
	// TODO
	return NULL;
	// Rewrite to be faster and take the underscores!
	//double value = strtod(parser.previous.start, NULL);
}

static void setup_parse_rules()
{
	static bool parse_rules_done = false;
	if (parse_rules_done) return;
	set_parse_rule(TOKEN_PAREN_L, grouping, call, PREC_CALL);
	set_parse_rule(TOKEN_DOT, NULL, NULL, PREC_CALL);
	set_parse_rule(TOKEN_MINUS, unary, binary, PREC_TERM);
	set_parse_rule(TOKEN_PLUS, NULL, binary, PREC_TERM);
	set_parse_rule(TOKEN_DIV, NULL, binary, PREC_FACTOR);
	set_parse_rule(TOKEN_MULT, NULL, binary, PREC_FACTOR);
	set_parse_rule(TOKEN_DOT, NULL, access, PREC_CALL);
	set_parse_rule(TOKEN_NOT, unary, NULL, PREC_NONE);
    set_parse_rule(TOKEN_BIT_NOT, unary, NULL, PREC_NONE);
	set_parse_rule(TOKEN_BIT_XOR, NULL, binary, PREC_BITWISE);
	set_parse_rule(TOKEN_BIT_OR, NULL, binary, PREC_BITWISE);
	set_parse_rule(TOKEN_BIT_AND, NULL, binary, PREC_BITWISE);
    set_parse_rule(TOKEN_EQUAL, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_NOT_EQUAL, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_GREATER, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_GREATER_EQUAL, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS_EQUAL, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LESS_EQUAL, NULL, binary, PREC_COMPARISON);
	set_parse_rule(TOKEN_LEFT_SHIFT, NULL, binary, PREC_SHIFT);
	set_parse_rule(TOKEN_RIGHT_SHIFT, NULL, binary, PREC_SHIFT);
	set_parse_rule(TOKEN_RIGHT_SHIFT_LOGIC, NULL, binary, PREC_SHIFT);
	set_parse_rule(TOKEN_INTEGER, integer_number, NULL, PREC_NONE);
	set_parse_rule(TOKEN_IDENTIFIER, identifier, NULL, PREC_NONE);
	set_parse_rule(TOKEN_POW, NULL, binary_right, PREC_POW);
	set_parse_rule(TOKEN_FLOAT, double_number, NULL, PREC_NONE);
	set_parse_rule(TOKEN_OR, NULL, or, PREC_OR);
	set_parse_rule(TOKEN_AND, NULL, and, PREC_OR);
    parse_rules_done = true;
}


bool parse(const char *source_text)
{
	setup_parse_rules();
	init_lexer(source_text);
	parser.module = NULL;
	parser.active_scope = 0;
	parser.had_error = false;
	parser.panic_mode = false;
	advance();
	source();
	consume(TOKEN_EOF, "Expect end of expression.");
	return !parser.had_error;
}
