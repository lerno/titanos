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
#define MAX_IDENTIFIER_LENGTH 31

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
	TOKEN_MACRO,
	TOKEN_PUBLIC,
	TOKEN_LOCAL,
	TOKEN_STRUCT, TOKEN_UNION, TOKEN_ENUM, TOKEN_SIZEOF,
    TOKEN_AT,
    TOKEN_AS,
    TOKEN_ERROR,
    TOKEN_EOF,

} TokenType;

const char *token_type_to_string(TokenType type);

static_assert(TOKEN_EOF < 128, "Too many different token types");


typedef struct _SourceLoc
{
	uint32_t id;
} SourceLoc;

extern const SourceLoc INVALID_LOC;

typedef struct _SourceRange
{
    SourceLoc loc;
    uint16_t length;
} SourceRange;

typedef struct
{
    uint32_t start;
    uint16_t length;
    uint16_t number;
} Line;

typedef struct _Token
{
    const char* start;
    SourceRange span;
	TokenType type : 8;
    union {
        const char *string;
    };
} Token;

typedef struct _File
{
	const char *contents;
	const char *name;
	SourceLoc start;
	SourceLoc end;
} File;

static_assert(sizeof(Token) == 32, "Invalid size of token");

void init_lexer(const char *filename, const char *source, size_t size);

Token scan_token(void);
Token lookahead(int steps);

bool token_compare_str(const Token *token1, const char *string);
void range_expand(SourceRange *to_update, Token *end_token);

const char *skip_to_end_of_previous_line(const char *file_start, const char *start);
const char *find_line_end(const char *line_start);

File *source_get_file(SourceLoc loc);

Token token_wrap(const char *string);


static inline bool is_lower(const char *text)
{
    return text[0] >= 'a' && text[0] <= 'z';
}

static inline bool is_upper(const char *text)
{
    return text[0] >= 'A' && text[0] <= 'Z';
}


