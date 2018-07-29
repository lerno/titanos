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

#define QUALIFIED_NAME_MAX 256


typedef struct
{
	Token start;
	Token end;
} QualifiedName;

typedef struct {
	char *name;
	uint8_t length;
} Symbol;

typedef struct _SymbolLink {
	Symbol symbol;
	struct SymbolLink *next;
} SymbolLink;

#define MAX_SYMBOLS 128

typedef struct
{
	uint8_t next_symbol;
	Symbol symbols[MAX_SYMBOLS];
	SymbolLink *symbol_link;
} SymList;

typedef struct _Scope
{
	SymList variable_symbols;
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

Parser parser;

static inline bool matches_symbol(Symbol *symbol, const char *name, int length)
{
	return symbol->length == length &&
	       (memcmp(symbol->name, name, (size_t)length) == 0);
}

static inline Symbol *find_symbol_in_symlist(SymList *sym_list, const char *name, int length)
{
	for (int i = sym_list->next_symbol - 1; i >= 0; i--)
	{
		Symbol *symbol = &sym_list->symbols[i];
		if (matches_symbol(symbol, name, length)) return symbol;
	}
	SymbolLink *symbol_link = sym_list->symbol_link;
	while (symbol_link != NULL)
	{
		if (matches_symbol(&symbol_link->symbol, name, length)) return &symbol_link->symbol;
	}
	return NULL;
}

static inline Symbol *push_symbol_in_symlist(SymList *sym_list, const char *name, int length)
{
	if (sym_list->next_symbol < MAX_SYMBOLS - 1)
	{
		return &sym_list->symbols[sym_list->next_symbol++];
	}
	SymbolLink *symbol_link = malloc(sizeof(SymbolLink));
	SymbolLink **pointer = &sym_list->symbol_link;
	while (pointer != NULL)
	{
		pointer = &((*pointer)->next);
	}
	*pointer = symbol_link;
	symbol_link->next = NULL;
	return &symbol_link->symbol;
}


static Symbol *find_variable_symbol(const char *name, int length)
{
	for (int i = parser.active_scope; i >= 0; i--)
	{
		Scope *scope = &parser.scopes[i];
		Symbol *symbol = find_symbol_in_symlist(&scope->variable_symbols, name, length);
		if (symbol) return symbol;
	}
	return NULL;
}

// Assumes we already checked for the symbol.
static Symbol *push_variable_symbol(const char *name, int length)
{
	return push_symbol_in_symlist(&parser.scopes[parser.active_scope].variable_symbols, name, length);
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
void proc(void);

static void set_parse_rule(token_type type, ParseFn prefix, ParseFn infix, precedence rule_precedence)
{
	rules[type].prefix = prefix;
	rules[type].precedence = rule_precedence;
	rules[type].prefix = prefix;
	rules[type].infix = infix;
};



// IDENTIFIER | proc_type -> type | void | type *
void type(void);

static void emit(const char *string)
{
	printf("%s", string);
}

static void emit_string(const char *string, int length)
{
	printf("%.*s", length, string);
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


static void expression_after_advance(void);

static void expression()
{
	advance();
	expression_after_advance();
}

static void return_statement()
{
	advance();
	emit("return ");
	expression_after_advance();
	emit(";");
}

static void assign_auto_stmt()
{
	if (find_variable_symbol(parser.previous.start, parser.previous.length))
	{
		error_at_current("Symbol redeclared");
		expression();
		return;
	}
	Symbol *symbol = push_variable_symbol(parser.previous.start, parser.previous.length);

	// Advance past ':'

	// Type handling
	expression();

	emit(";");
}

static void assign_stmt()
{
	if (find_variable_symbol(parser.previous.start, parser.previous.length))
	{
		error_at_current("Symbol redeclared");
		expression();
		return;
	}
	Symbol *symbol = push_variable_symbol(parser.previous.start, parser.previous.length);
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
			break;
		case TOKEN_COLON:
			assign_stmt();
			break;
		default:
			expression_after_advance();
			emit(";");
			break;
	}
}

bool no_block_statement()
{
	switch (parser.current.type)
	{
		case TOKEN_RETURN:
			return_statement();
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
	emit("while(");
	consume(TOKEN_PAREN_L, "Expected (");
	expression();
	emit(")");
	consume(TOKEN_PAREN_R, "Expected )");
	block_body();
}

static void until_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	emit("do {");
	expression();
	consume(TOKEN_PAREN_R, "Expected )");
	block_body();
	emit("} while (");
	emit(")");
}

static void if_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	emit("if (");
	expression();
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

void proc()
{
	advance();
	consume(TOKEN_IDENTIFIER, "Expected name");
	Token identifier = parser.previous;
	consume(TOKEN_PAREN_L, "Expected (");
	named_var_list();
	consume(TOKEN_PAREN_R, "Expected )");
	consume(TOKEN_ARROW, "Expected ->");
	type();
	emit(" ");
	emit_string(identifier.start, identifier.length);
	emit("(TODO)");
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

static void parse_precedence_after_advance(precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = get_rule(parser.previous.type)->prefix;
	if (prefix_rule == NULL)
	{
		error("Expected expression.");
		return;
	}

	emit("(");
	prefix_rule();
	emit(")");
	while (precedence <= get_rule(parser.current.type)->precedence)
	{
		advance();
		ParseFn infix_rule = get_rule(parser.previous.type)->infix;
		emit("(");
		infix_rule();
		emit(")");
	}
}

static void parse_precedence(precedence precedence)
{
	advance();
	return parse_precedence_after_advance(precedence);
}

static void expression_after_advance()
{
	scratch_buffer_clear();
	parse_precedence_after_advance(PREC_ASSIGNMENT);
}

static void binary(void)
{
	// Remember the operator.
	token_type operator_type = parser.previous.type;

	// Compile the right operand.

	switch (operator_type)
	{
		case TOKEN_MULT:
			emit(" * ");
			break;
		default:
			error_at_current("Unknown operator");
	}

	ParseRule *rule = get_rule(operator_type);
	parse_precedence((precedence)(rule->precedence + 1));
	// Right hand side now loaded.

}


// Right associative
static void binary_right()
{
	// Remember the operator.
	token_type operator_type = parser.previous.type;


	// Compile the right operand.
	ParseRule *rule = get_rule(operator_type);
	parse_precedence((precedence)(rule->precedence));
	// Right hand side now loaded.

	error_at_current("Unsupported");
}


static void unary()
{
	token_type operatorType = parser.previous.type;


	// Compile the operand.
	emit_string(parser.previous.start, parser.previous.length);
	parse_precedence(PREC_UNARY);
}

static void identifier()
{
	emit_string(parser.previous.start, parser.previous.length);
}

static void grouping()
{
	emit("(");
	expression();
	consume(TOKEN_PAREN_R, "Expected ')' after expression.");
	emit(")");
}

static void call()
{
	emit("(");
	if (parser.current.type != TOKEN_PAREN_R)
    {
        do
        {
        	expression();
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
	        return call(expression);
        case TOKEN_BRACKET_L: // TODO
	        return;
        case TOKEN_DOT:
	        return access(expression);
        default:
	        return parse_precedence_after_advance(PREC_CALL);
    }
}

static void or()
{
	emit(" || ");
	parse_precedence(PREC_OR);
}

static void and(void)
{
	emit(" && ");
	parse_precedence(PREC_AND);
}


static void integer_number(void)
{
	uint64_t number = parse_uint64(parser.previous.start, parser.previous.length);
	emit("NUMBER");
}


static void double_number(void)
{
	emit("DOUBLE");
	// TODO
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
