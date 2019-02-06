#pragma once
//
//  lexer.h
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//


#include "common.h"

#define TOKEN_MAX_LENGTH 0xFFFF

typedef enum
{
    // Single-character tokens.
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COMMA,
    TOKEN_HASH,
    TOKEN_DOLLAR,
    TOKEN_DOT,
    TOKEN_EOS,
    
    // One or two character tokens.
    TOKEN_PLUS,
    TOKEN_PLUSPLUS,
    TOKEN_PLUS_ASSIGN,
    TOKEN_BIT_NOT,
    TOKEN_NOT,
    TOKEN_MINUS,
    TOKEN_MINUSMINUS,
    TOKEN_MINUS_ASSIGN,
    TOKEN_STAR,
    TOKEN_MULT_ASSIGN,
    TOKEN_POW,
    TOKEN_DIV,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD,
    TOKEN_MOD_ASSIGN,
    TOKEN_NOT_EQUAL,
    TOKEN_EQ,
    TOKEN_EQEQ,
    TOKEN_COLON,
    TOKEN_COLON_ASSIGN,
    TOKEN_COLCOLON,
    TOKEN_DOTDOT,

    // Three or more
    TOKEN_ELIPSIS,
    TOKEN_GREATER,
    TOKEN_GREATER_EQ,
    TOKEN_RIGHT_SHIFT,
    TOKEN_RIGHT_SHIFT_ASSIGN,
    TOKEN_RIGHT_SHIFT_LOGIC,
    TOKEN_RIGHT_SHIFT_LOGIC_ASSIGN,
    TOKEN_LESS,
    TOKEN_LESS_EQ,
    TOKEN_LEFT_SHIFT,
    TOKEN_LEFT_SHIFT_ASSIGN,
	TOKEN_ARROW,
    TOKEN_AND,
    TOKEN_AND_ASSIGN,
    TOKEN_AMP,
    TOKEN_BIT_AND_ASSIGN,
    TOKEN_OR,
    TOKEN_OR_ASSIGN,
    TOKEN_BIT_OR,
    TOKEN_BIT_OR_ASSIGN,
    TOKEN_BIT_XOR,
    TOKEN_BIT_XOR_ASSIGN,
    TOKEN_NO_INIT,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_INTEGER,
    TOKEN_FLOAT,

    TOKEN_QUESTION,
    TOKEN_ELVIS,
    
    // Keywords.
	TOKEN_VOID, TOKEN_ALIAS,
	TOKEN_CONST, TOKEN_VOLATILE,
    TOKEN_ELSE, TOKEN_FALSE, TOKEN_CONTINUE,
    TOKEN_FUNC, TOKEN_FOR, TOKEN_IMPORT, TOKEN_MODULE, TOKEN_IF, TOKEN_NIL,
    TOKEN_RETURN, TOKEN_GOTO, TOKEN_DEFER,
    TOKEN_TRUE, TOKEN_WHILE, TOKEN_CASE, TOKEN_ASM, TOKEN_DEFAULT, TOKEN_CAST,
	TOKEN_SWITCH, TOKEN_UNTIL, TOKEN_BREAK, TOKEN_TYPE,
	TOKEN_DO,
	TOKEN_PUBLIC,
	TOKEN_LOCAL,
	TOKEN_STRUCT, TOKEN_UNION, TOKEN_ENUM, TOKEN_SIZEOF,
    TOKEN_AT,
    TOKEN_AS,
    TOKEN_ERROR,
    TOKEN_EOF,

} token_type;

const char *token_type_to_string(token_type type);

static_assert(TOKEN_EOF < 128, "Too many different token types");

#define SPLAT_TOK(_tok) (int)((_tok).length), (_tok).start

typedef struct _Token
{
    const char* start;
	uint16_t length;
	uint16_t source_file;
	token_type type : 8;
    unsigned line : 24;
} Token;

typedef struct _File
{
	const char *contents;
	const char *name;
} File;

static_assert(sizeof(Token) == 16, "Invalid size of token");

void init_lexer(const char *filename, const char *source);
File *token_get_file(Token *token);
Token scan_token(void);
Token lookahead(int steps);
const char *line_start(Token *token);
bool token_compare(const Token *token1, const Token *token2);
bool token_compare_str(const Token *token1, const char *string);
void token_expand(Token *to_expand, Token *end_token);

const char *skip_to_end_of_previous_line(const char *file_start, const char *start);
const char *find_line_start(const char *file_start, const char *start);
const char *find_line_end(const char *line_start);

Token token_wrap(const char *string);

static inline bool is_lower(Token *token)
{
    return token->start[0] >= 'a' && token->start[0] <= 'z';
}

static inline bool is_upper(Token *token)
{
    return token->start[0] >= 'A' && token->start[0] <= 'Z';
}

static inline bool exceeds_identifier_len(const Token *token)
{
	return token->length > 31;
}
