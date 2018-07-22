#pragma once
//
//  lexer.h
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//

typedef enum
{
    // Single-character tokens.
    TOKEN_PAREN_L,
    TOKEN_PAREN_R,
    TOKEN_BRACE_L,
    TOKEN_BRACE_R,
    TOKEN_BRACKET_L,
    TOKEN_BRACKET_R,
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
    TOKEN_MULT,
    TOKEN_MULT_ASSIGN,
    TOKEN_POW,
    TOKEN_DIV,
    TOKEN_DIV_ASSIGN,
    TOKEN_NOT_EQUAL,
    TOKEN_ASSIGN,
    TOKEN_EQUAL,
    TOKEN_COLON,
    TOKEN_COLON_ASSIGN,
    TOKEN_COLCOLON,

    // Three or more
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_RIGHT_SHIFT,
    TOKEN_RIGHT_SHIFT_ASSIGN,
    TOKEN_RIGHT_SHIFT_LOGIC,
    TOKEN_RIGHT_SHIFT_LOGIC_ASSIGN,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
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
	TOKEN_VOID,
	TOKEN_EXTEND,
    TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE, TOKEN_CONTINUE,
    TOKEN_PROC, TOKEN_FOR, TOKEN_IMPORT, TOKEN_MODULE, TOKEN_PROGRAM, TOKEN_IF, TOKEN_NIL,
    TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
    TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,
	TOKEN_SWITCH, TOKEN_UNTIL,
	TOKEN_STRUCT,
    TOKEN_AT,
    TOKEN_ERROR,
    TOKEN_EOF,
    
} token_type;

typedef struct
{
    token_type type;
    const char* start;
    int length;
    int line;
} Token;

void init_lexer(const char *source);
Token scan_token(void);
