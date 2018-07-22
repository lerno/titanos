#pragma once
//
//  lexer.h
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//


#include "common.h"

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
    TOKEN_BIT_AND,
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

typedef struct _Token
{
    token_type type;
    const char* start;
    uint32_t length;
    int line;
} Token;

void init_lexer(const char *source);
Token scan_token(void);
Token lookahead(int steps);
const char *line_start(Token *token);
const char *line_end(Token *token);
bool token_compare(Token *token1, Token *token2);
bool token_compare_str(Token *token1, const char *string);

static inline bool is_lower(Token *token)
{
    return token->start[0] >= 'a' && token->start[0] <= 'z';
}

static inline bool is_upper(Token *token)
{
    return token->start[0] >= 'A' && token->start[0] <= 'Z';
}
