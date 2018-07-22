//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include "parser.h"
#include "lexer.h"
#include "debug.h"


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

Parser parser;

void import();


void proc();


// IDENTIFIER | proc_type -> type | void | type *
void type()
;


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

void block()
{
	consume(TOKEN_BRACE_L, "Expected {");
	error_at_current("Unhandled");
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
	block();
}

void program()
{
	error_at_current("Unhandled");
	advance();
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



bool parse(const char *source_text)
{
	// setup_parse_rules();
	init_lexer(source_text);
	parser.had_error = false;
	parser.panic_mode = false;
	advance();
	source();
	consume(TOKEN_EOF, "Expect end of expression.");
	return !parser.had_error;
}
