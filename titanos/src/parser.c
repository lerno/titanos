//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include <assert.h>
#include "parser.h"
#include "lexer.h"
#include "debug.h"
#include "string_utils.h"
#include "fnv.h"
#include <memory.h>
#include "common.h"
#include "scratch_buffer.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <inttypes.h>
#include "expression.c"
#include "type.c"
#include "symbol.c"

#define QUALIFIED_NAME_MAX 256


typedef struct
{
	Token start;
	Token end;
} QualifiedName;

typedef struct _Scope
{
	SymList variable_symbols;
} Scope;

#define MAX_SCOPES 128


#define EMIT_BUFFER 0xFFFF
#define EXPRESSION_STACK_DEPTH 1024

typedef struct
{
	Scope scopes[MAX_SCOPES];
	char *module;
	char *module_prefix;
	Token current;
	Token previous;
	bool had_error;
	bool panic_mode;
	uint8_t active_scope;
	Expression expression_stack[EXPRESSION_STACK_DEPTH];
	uint32_t expression_stack_pointer;
} Parser;

Parser parser;

Type TYPE_BOOL;
Type TYPE_F64;
Type TYPE_U64;
Type TYPE_I64;

static Type *find_type(Token *token)
{
	return NULL;
}

static Symbol *find_variable_symbol(Token *token)
{
	for (int i = parser.active_scope; i >= 0; i--)
	{
		Scope *scope = &parser.scopes[i];
		Symbol *symbol = find_symbol_in_symlist(&scope->variable_symbols, token->start, token->length);
		if (symbol) return symbol;
	}
	return NULL;
}

// Assumes we already checked for the symbol.
static Symbol *push_variable_symbol(Token *token)
{
	return push_symbol_in_symlist(&parser.scopes[parser.active_scope].variable_symbols, token->start, token->length);
}

typedef void (*ParseFn)(void);

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
	ParseFn prefix;
	ParseFn infix;
	precedence precedence;
} ParseRule;


static ParseRule rules[TOKEN_EOF + 1];


void import(void);
void proc_stmt(void);

static void set_parse_rule(token_type type, ParseFn prefix, ParseFn infix, precedence rule_precedence)
{
	rules[type].prefix = prefix;
	rules[type].precedence = rule_precedence;
	rules[type].prefix = prefix;
	rules[type].infix = infix;
};



// IDENTIFIER | proc_type -> type | void | type *

static void emit(const char *string)
{
	printf("%s", string);
}

static void emit_string(const char *string, int length)
{
	printf("%.*s", length, string);
}

static void emit_token(Token *token)
{
	printf("%.*s", token->length, token->start);
}



static inline Expression *new_expression()
{
	if (parser.expression_stack_pointer == EXPRESSION_STACK_DEPTH)
	{
		error_at_current("Max stack deptch reached!");
		return malloc(sizeof(Expression));
	}
	return &parser.expression_stack[parser.expression_stack_pointer++];
}

static void expression_stack_push_binary(Token token)
{
	Expression *expression = new_expression();
	expression->type = EXPRESSION_BINARY;
	expression->is_constant = false;
	expression->value.operator = token;
}

static void expression_stack_push_unary(Token token)
{
	Expression *expression = new_expression();
	expression->type = EXPRESSION_UNARY;
	expression->is_constant = false;
	expression->value.operator = token;
}

static void expression_stack_pop_to(Expression *expression)
{
	parser.expression_stack_pointer = (uint32_t)(expression - &parser.expression_stack[0] + 1);
}

static void expression_stack_clear()
{
	parser.expression_stack_pointer = 0;
}

static Expression *expression_stack_top()
{
	return &parser.expression_stack[parser.expression_stack_pointer - 1];
}

static Expression *expression_stack_pop()
{
	assert(parser.expression_stack_pointer > 0 && "Tried to pop empty stack");
	return &parser.expression_stack[--parser.expression_stack_pointer];
}

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


void error_at_current(const char *message)
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
	char *result = malloc((size_t)(position + 1));
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
	parser.module_prefix = path_to_underscore_prefix(parser.module);
	consume(TOKEN_EOS, "Expected ;");
	recover_to(TOKEN_EOS);
}

static Type *type(void);

static void var_list(void)
{
	type();
	while (parser.current.type == TOKEN_COMMA)
	{
		type();
	}
}

// PROC ( type_list ) -> type
Type *proc_type()
{
	// Skip "proc"
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	var_list();
	consume(TOKEN_PAREN_R, "Expected )");
	consume(TOKEN_ARROW, "Expected ->");
	type();
	return NULL;
}

Type *primitive_type(void)
{
	Type *type = find_type(&parser.current);
	advance();
	return type;
}

Type *complex_type(void)
{
	Type *type;
	switch (parser.current.type)
	{
		case TOKEN_PROC:
			type = proc_type();
			break;
		case TOKEN_IDENTIFIER:
			type = primitive_type();
			break;
		case TOKEN_PAREN_L:
			advance();
			type = complex_type();
			consume(TOKEN_PAREN_R, "Expected )");
			break;
		default:
			error_at_current("Expected a type");
			return &ERROR_TYPE;
	}
	while (parser.current.type == TOKEN_MULT)
	{
		type = type_get_pointer_to_type(type);
		advance();
	}
	return type;
}
// IDENTIFIER | proc_type -> type | void | type *
static Type *type()
{
	if (parser.current.type == TOKEN_VOID)
	{
		advance();
		return &VOID_TYPE;
	}
	return complex_type();
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


static void expression_after_advance();

static void expression_stmt()
{
	advance();
	expression_after_advance();
}


static void emit_expression(Expression *expression)
{
	Expression *left;
	Expression *right;
	switch (expression->type)
	{
		case EXPRESSION_BOOL:
			emit(expression->value.b ? " true " : " false ");
			break;
		case EXPRESSION_INTEGER:
			printf("%" PRId64, expression->value.si);
			break;
		case EXPRESSION_UINTEGER:
			printf("%" PRIu64, expression->value.si);
			break;
		case EXPRESSION_FLOAT:
			printf("%Lf", expression->value.f);
			break;
		case EXPRESSION_UNARY:
			emit("(");
			emit_token(&expression->value.operator);
			emit_expression(expression_stack_top());
			emit(")");
			break;
		case EXPRESSION_BINARY:
			emit("(");
			emit_expression(expression_stack_top());
			emit_token(&expression->value.operator);
			emit_expression(expression_stack_top());
			emit(")");
        case EXPRESSION_IDENTIFIER:
            emit_string(expression->value.variable->name, expression->value.variable->length);
            break;
		default:
			exit(1);
	}
}

static void emit_expression_stack()
{
	while (parser.expression_stack_pointer > 0)
	{
		emit_expression(expression_stack_pop());
	}
}

static void return_statement()
{
	advance();
	emit("return ");
	expression_stack_clear();
	advance();
	expression_after_advance();
	if (parser.had_error) return;
	emit_expression_stack();
	emit(";");
}



static Type *expression_type(int *pointer)
{
	--(*pointer);
	Expression *expression = &parser.expression_stack[*pointer];
	Type *rhs;
	Type *lhs;
	switch (expression_stack_top()->type)
	{
		case EXPRESSION_FLOAT:
			return &TYPE_F64;
		case EXPRESSION_BOOL:
			return &TYPE_BOOL;
		case EXPRESSION_UINTEGER:
			return &TYPE_U64;
		case EXPRESSION_INTEGER:
			return &TYPE_I64;
		case EXPRESSION_UNARY:
			rhs = expression_type(pointer);
			switch (expression->value.operator.type)
			{
				case TOKEN_MINUS:
					return rhs;
				case TOKEN_NOT:
					return &TYPE_BOOL;
				case TOKEN_BIT_AND:
					return rhs; // FIX TODO make pointer;
				case TOKEN_MULT:
					return rhs; // FIX TODO deref;
				default:
					assert(false && "Not possible");
					return NULL;
			}
		case EXPRESSION_BINARY:
			rhs = expression_type(pointer);
			lhs = expression_type(pointer);
			switch (expression->value.operator.type)
			{
				case TOKEN_MINUS:
				case TOKEN_PLUS:
				case TOKEN_BIT_AND:
				case TOKEN_MULT:
					return lhs;
				default:
					assert(false && "Not possible");
					return NULL;
			}
		case EXPRESSION_IDENTIFIER:
			return expression->value.variable->type;
		case EXPRESSION_ERROR:
			return &VOID_TYPE;
	}

}
static Type *expression_stack_type()
{
	if (!parser.expression_stack_pointer) return &VOID_TYPE;
	int pointer = parser.expression_stack_pointer;
	return expression_type(&pointer);
}

static void assign_auto_stmt()
{
	if (find_variable_symbol(&parser.previous))
	{
		error_at_current("Symbol redeclared");
		expression_stmt();
		return;
	}
	Symbol *symbol = push_variable_symbol(&parser.previous);

	// Advance past ':='
	advance();

	// Type handling
	expression_stack_clear();
	expression_stmt();
	symbol->type = expression_stack_type();
	//emit_type(symbol->type);
	emit(" ");
	emit_string(symbol->name, symbol->length);
	emit(";\n");
	emit_string(symbol->name, symbol->length);
	emit("=");
	emit_expression_stack();
	emit(";");
}

static void assign_stmt()
{
	expression_stack_pop();

	if (find_variable_symbol(&parser.previous))
	{
		error_at_current("Symbol redeclared");
		expression_stmt();
		return;
	}
	Symbol *symbol = push_variable_symbol(&parser.previous);

	// Advance past ':'
	advance();
	// Parse type

	// symbol->type = type();

	// emit: int foo;

	if (parser.current.type == TOKEN_EOS) return;

	// emit foo =
	// type = expression();
	expression_after_advance();

	emit(";");
}

static void variable_declaration_or_expression()
{
	advance();
	switch (parser.current.type)
	{
		case TOKEN_COLON_ASSIGN:
			assign_auto_stmt();
			return;
		case TOKEN_COLON:
			assign_stmt();
			return;
		default:
			break;
	}
	Expression expr;
	expression_after_advance(&expr);
	emit_expression(&expr);
	emit(";");
}

bool no_block_statement()
{
	switch (parser.current.type)
	{
		case TOKEN_RETURN:
			return_statement();
			consume(TOKEN_EOS, "Expected ';'");
			return true;
		case TOKEN_IDENTIFIER:
			variable_declaration_or_expression();
			return true;
		case TOKEN_EOS:
			return true;
		default:
			return false;
	}
}

static void braced_statements(void);


static void block_body()
{
	if (parser.current.type == TOKEN_ARROW)
	{
		emit("\n{\n");
		no_block_statement();
		emit("\n}\n");
		return;
	}
	braced_statements();
}

static void while_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	Expression expr;
	expression_stmt(&expr);
	consume(TOKEN_PAREN_R, "Expected )");
	emit("while(");
	emit_expression(&expr);
	emit(")");
	block_body();
}

static void until_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	Expression expr;
	expression_stmt(&expr);
	consume(TOKEN_PAREN_R, "Expected )");
	emit("do {");
	block_body();
	emit("} while (");
	emit_expression(&expr);
	emit(")");
}

static void if_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	Expression expr;
	expression_stmt(&expr);
	consume(TOKEN_PAREN_R, "Expected )");
	emit("if (");
	emit_expression(&expr);
	consume(TOKEN_PAREN_R, "Expected )");
	emit(")");
	if (parser.current.type == TOKEN_ARROW)
	{
		emit("{");
		no_block_statement();
		emit("}");
	}
	else
	{
		braced_statements();
		if (parser.current.type == TOKEN_ELSE)
		{
			emit(" else ");
			advance();
			braced_statements();
		}
	}
}

bool statement()
{
	switch (parser.current.type)
	{
		case TOKEN_IF:
			if_statement();
			break;
		case TOKEN_WHILE:
			while_statement();
			break;
		case TOKEN_UNTIL:
			until_statement();
			break;
			/*
		case TOKEN_FOR:
			for_statement();
			break;
		case TOKEN_SWITCH:
			switch_statement();
			break;*/
		default:
			return no_block_statement();
	}
	return true;
}
void statements()
{
	while (statement()) {}
}

static void braced_statements()
{
	consume(TOKEN_BRACE_L, "Expected {");
	emit("\n{\n");
	statements();
	emit("\n}\n");
	consume(TOKEN_BRACE_R, "Expected }");
}

void proc_stmt()
{
	advance();
	consume(TOKEN_IDENTIFIER, "Expected name");
	Token identifier = parser.previous;
	if (parser.module)
	{
		emit(parser.module_prefix);
	}
	consume(TOKEN_PAREN_L, "Expected (");
	named_var_list();
	consume(TOKEN_PAREN_R, "Expected )");
	consume(TOKEN_ARROW, "Expected ->");
	type();
	braced_statements();
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
		proc_stmt();
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
				proc_stmt();
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

static void parse_precedence_after_advance(precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = get_rule(parser.previous.type)->prefix;
	if (prefix_rule == NULL)
	{
		error("Expected expression.");
		return;
	}

	prefix_rule();
	while (precedence <= get_rule(parser.current.type)->precedence)
	{
		advance();
		ParseFn infix_rule = get_rule(parser.previous.type)->infix;
		infix_rule();
	}
}

static void parse_precedence(precedence precedence)
{
	advance();
	return parse_precedence_after_advance(precedence);
}

static void expression_after_advance()
{
	parse_precedence_after_advance(PREC_ASSIGNMENT);
}

static void binary()
{
	// Remember the operator.
	Token token = parser.previous;
	token_type operator_type = parser.previous.type;

	Expression *left_hand = expression_stack_top();

	ParseRule *rule = get_rule(operator_type);
	parse_precedence((precedence)(rule->precedence + 1));

	Expression *right_side = expression_stack_top();
	// Compile the right operand.

	if (right_side->is_constant && left_hand->is_constant)
	{
		expression_stack_pop_to(left_hand);
		switch (operator_type)
		{
			case TOKEN_MULT:
				constant_fold_mult(left_hand, right_side);
				return;
			case TOKEN_MINUS:
				constant_minus(right_side);
				constant_fold_plus(left_hand, right_side);
				return;
			case TOKEN_PLUS:
				expression_stack_pop_to(left_hand);
				constant_fold_plus(left_hand, right_side);
				return;
			default:
				error_at_current("Unknown operator");
				return;
		}
	}
	expression_stack_push_binary(token);
}




// Right associative
static void binary_right()
{
	Token token = parser.previous;
	// Remember the operator.
	token_type operator_type = parser.previous.type;

	Expression *left_hand = expression_stack_top();

	// Compile the right operand.
	ParseRule *rule = get_rule(operator_type);
	parse_precedence((precedence)(rule->precedence));

	Expression *right_hand = expression_stack_top();

	// Right hand side now loaded.

	bool constant_fold = right_hand->is_constant && left_hand->is_constant;


	switch (operator_type)
	{
		case TOKEN_POW:
			if (constant_fold)
			{
				expression_stack_pop_to(left_hand);
				if (!constant_pow(left_hand, right_hand))
				{
					error_at_current("Illegal type for exponentiation");
				}
				return;
			}
			expression_stack_push_binary(token);
			break;
		default:
			error_at_current("Unknown operator");
	}
}




static void unary()
{
	Token token = parser.previous;
	token_type operator_type = parser.previous.type;

	// Compile the operand.
	emit_string(parser.previous.start, parser.previous.length);
	parse_precedence(PREC_UNARY);

	Expression *expression = expression_stack_top();

	if (expression->is_constant)
	{
		switch (operator_type)
		{
			case TOKEN_MINUS:
				constant_minus(expression);
				break;
			case TOKEN_NOT:
				constant_not(expression);
				break;
			case TOKEN_MULT:
				// DOTHIS;
				break;
			case TOKEN_BIT_AND:
				// Addrof;
				break;
			case TOKEN_BIT_NOT:
				if (!constant_bit_not(expression))
				{
					error_at_current("Illegal type for bit-wise not");
				}
				break;
			default:
				assert(false && "Unkown unary token");
				break;
		}
		return;
	}

	expression_stack_push_unary(token);
}

static void identifier()
{
	Expression *expr = new_expression();
	expr->type = EXPRESSION_IDENTIFIER;
	expr->is_constant = false;
	Symbol *symbol = find_variable_symbol(&parser.previous);
	if (symbol == NULL)
	{
		error_at_current("No such variable found");
		expr->type = EXPRESSION_ERROR;
	}
	expr->value.variable = symbol;
}

static void grouping()
{
	expression_stmt();
	consume(TOKEN_PAREN_R, "Expected ')' after expression.");
}

static void call()
{
	emit("(");
	if (parser.current.type != TOKEN_PAREN_R)
    {
        do
        {
	        expression_stmt();
	        emit(",");
        } while (parser.current.type == TOKEN_COMMA);
    }
    consume(TOKEN_PAREN_R, "Expected )");
	emit(")");
}

static void access()
{
	// TODO BROOOKEN
    consume(TOKEN_IDENTIFIER, "Expected identifier");
    // Three possibilities: () [] .
    switch (parser.current.type)
    {
        case TOKEN_PAREN_L:
	        return call();
        case TOKEN_BRACKET_L: // TODO
	        return;
        case TOKEN_DOT:
	        return access();
        default:
	        return parse_precedence_after_advance(PREC_CALL);
    }
}

static void or()
{
	Expression *left_hand = expression_stack_top();
	Token token = parser.previous;
	parse_precedence(PREC_OR);
	if (left_hand->is_constant)
	{
		constant_bool_cast(left_hand);
		if (left_hand->value.b)
		{
			expression_stack_pop_to(left_hand);
			return;
		}
	}
	expression_stack_push_binary(token);
}

static void and(Expression *expression)
{
	Expression *left_hand = expression_stack_top();
	Token token = parser.previous;
	if (left_hand->is_constant)
	{
		constant_bool_cast(left_hand);
		if (!left_hand->value.b)
		{
			expression_stack_pop_to(left_hand);
			expression_stack_pop();
			parse_precedence(PREC_AND);
			return;
		}
	}
	parse_precedence(PREC_AND);
	expression_stack_push_binary(token);
}


static void integer_number()
{
	Expression *expr = new_expression();
	expr->value.ui = parse_uint64(parser.previous.start, parser.previous.length);
	expr->is_constant = true;
	expr->type = EXPRESSION_UINTEGER;
}


static void double_number()
{
	Expression *expr = new_expression();
	expr->value.f = strtod(parser.previous.start, NULL);
	expr->is_constant = true;
	expr->type = EXPRESSION_FLOAT;
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
