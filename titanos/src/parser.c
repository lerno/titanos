//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include "parser.h"
#include "lexer.h"
#include "debug.h"
#include "string.h"


typedef struct
{
	Token start;
	Token end;
} QualifiedName;

typedef struct
{
	QualifiedName module;
	Token current;
	Token previous;
	bool had_error;
	bool panic_mode;
} Parser;

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

Parser parser;

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

static QualifiedName qualified_name()
{
	Token start = parser.current;
	consume(TOKEN_IDENTIFIER, "Expected name");
	while (parser.current.type == TOKEN_COLCOLON)
	{
		advance();
		consume(TOKEN_IDENTIFIER, "Expected name");
	}
	QualifiedName qualified_name = { .start = start, .end = parser.previous };
	return qualified_name;
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
	// TODO properly handle the module
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

void return_statement()
{
	advance();
	advance();
	expression_after_advance();
}

bool no_block_statement()
{
	switch (parser.current.type)
	{
		case TOKEN_RETURN:
			return_statement();
			return true;
		default:
			return false;
	}
}

static void braced_statements(void);

void block_body()
{
	if (parser.current.type == TOKEN_ARROW)
	{
		if (!no_block_statement())
		{
			error_at_current("Expected statement");
		}
		consume(TOKEN_EOS, "Expected ;");
		return;
	}
	braced_statements();
}

void while_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	expression();
	consume(TOKEN_PAREN_R, "Expected )");
	block_body();
}

void until_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	expression();
	consume(TOKEN_PAREN_R, "Expected )");
	block_body();
}

void if_statement()
{
	advance();
	consume(TOKEN_PAREN_L, "Expected (");
	expression();
	consume(TOKEN_PAREN_R, "Expected )");
	if (parser.current.type == TOKEN_ARROW)
	{
		if (!no_block_statement())
		{
			error_at_current("Expected statement");
		}
		consume(TOKEN_EOS, "Expected ;");
		return;
	}
	braced_statements();
	if (parser.current.type == TOKEN_ELSE)
	{
		advance();
		braced_statements();
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
			if (!no_block_statement()) return false;
			consume(TOKEN_EOS, "Expected ;");
	}
	return true;
}
void statements()
{
	while (statement()) {};
}

void braced_statements()
{
	consume(TOKEN_BRACE_L, "Expected {");
	statements();
	consume(TOKEN_BRACE_R, "Expected }");
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
	parse_precedence_after_advance(precedence);
}

static void expression_after_advance()
{
	parse_precedence_after_advance(PREC_ASSIGNMENT);
}

static void binary()
{
	// Remember the operator.
	token_type operator_type = parser.previous.type;

	// Compile the right operand.
	ParseRule *rule = get_rule(operator_type);
	parse_precedence((precedence)(rule->precedence + 1));
	// Right hand side now loaded.

	// Emit the operator instruction.
	switch (operator_type)
	{
		case TOKEN_RIGHT_SHIFT:
			break;
		case TOKEN_LEFT_SHIFT:
			break;
		case TOKEN_RIGHT_SHIFT_LOGIC:
			break;
		case TOKEN_BIT_AND:
			break;
		case TOKEN_BIT_OR:
			break;
		case TOKEN_BIT_XOR:
			break;
		case TOKEN_PLUS:
			break;
		case TOKEN_MINUS:
			break;
		case TOKEN_MULT:
			break;
		case TOKEN_DIV:
			break;
		case TOKEN_POW:
			break;
		case TOKEN_GREATER_EQUAL:
			break;
		case TOKEN_GREATER:
			break;
		case TOKEN_LESS:
			break;
		case TOKEN_LESS_EQUAL:
			break;
		case TOKEN_EQUAL:
			break;
		case TOKEN_NOT_EQUAL:
			break;
		default:
			printf("Invalid binary operator %d\n", operator_type);
			return; // Unreachable.
	}
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

	// Emit the operator instruction.
	switch (operator_type)
	{
		case TOKEN_POW:
			break;
		default:
			printf("Invalid binary-right operator %d\n", operator_type);
			return; // Unreachable.
	}
}


static void unary()
{
	token_type operatorType = parser.previous.type;

	// Compile the operand.
	parse_precedence(PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType)
	{
		case TOKEN_MINUS:
			break;
		case TOKEN_NOT:
			break;
		case TOKEN_BIT_NOT:
			break;
		default:
			return; // Unreachable.
	}
}

static void identifier()
{
}

static void grouping()
{
	expression();
	consume(TOKEN_PAREN_R, "Expected ')' after expression.");
}

static void call()
{
    if (parser.current.type != TOKEN_PAREN_R)
    {
        do
        {
            expression();
        } while (parser.current.type == TOKEN_COMMA);
    }
    consume(TOKEN_PAREN_R, "Expected )");
}

static void access()
{
    consume(TOKEN_IDENTIFIER, "Expected identifier");
    // Three possibilities: () [] .
    switch (parser.current.type)
    {
        case TOKEN_PAREN_L:
            call();
            break;
        case TOKEN_BRACKET_L: // TODO
            break;
        case TOKEN_DOT:
            access();
            break;
        default:
	        parse_precedence_after_advance(PREC_CALL);
            break;
    }
}

static void or()
{
	parse_precedence(PREC_OR);
}

static void and()
{
	parse_precedence(PREC_AND);
}


static void integer_number(void)
{
	uint64_t number = parse_uint64(parser.previous.start, parser.previous.length);
}


static void double_number(void)
{
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
	parser.had_error = false;
	parser.panic_mode = false;
	advance();
	source();
	consume(TOKEN_EOF, "Expect end of expression.");
	return !parser.had_error;
}
