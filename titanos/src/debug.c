//
//  debug.c
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//

#include "debug.h"
#include <string.h>
#include <stdio.h>

char *token_name(Token *token)
{
    static char buffer[65536];
    switch (token->type)
    {
        case TOKEN_COLCOLON:
            return "::";
        case TOKEN_DOLLAR:
            return "$";
        case TOKEN_HASH:
            return "#";
        case TOKEN_ELVIS:
            return "?:";
        case TOKEN_QUESTION:
            return "?";
        case TOKEN_PAREN_L:
            return "( \0";
        case TOKEN_PAREN_R:
            return " )";
        case TOKEN_BRACE_L:
            return "{ ";
        case TOKEN_BRACE_R:
            return " }";
        case TOKEN_BRACKET_L:
            return "[ ";
        case TOKEN_BRACKET_R:
            return " ]";
        case TOKEN_COMMA:
            return ", ";
        case TOKEN_DOT:
            return ".";
        case TOKEN_EOS:
            return ";\n";
        case TOKEN_PLUS:
            return "+";
        case TOKEN_PLUSPLUS:
            return "++";
        case TOKEN_PLUS_ASSIGN:
            return "+=";
        case TOKEN_BIT_NOT:
            return "~";
        case TOKEN_NOT:
            return "!";
        case TOKEN_MINUS:
            return "-";
        case TOKEN_MINUSMINUS:
            return "--";
        case TOKEN_MINUS_ASSIGN:
            return "-=";
        case TOKEN_MULT:
            return "*";
        case TOKEN_MULT_ASSIGN:
            return "*=";
        case TOKEN_POW:
            return "**";
        case TOKEN_DIV:
            return "/";
        case TOKEN_DIV_ASSIGN:
            return "/=";
        case TOKEN_NOT_EQUAL:
            return "!=";
        case TOKEN_ASSIGN:
            return "=";
        case TOKEN_EQUAL:
            return "==";
        case TOKEN_COLON:
            return ":";
        case TOKEN_COLON_ASSIGN:
            return ":=";
        case TOKEN_GREATER:
            return ">";
        case TOKEN_GREATER_EQUAL:
            return ">=";
        case TOKEN_RIGHT_SHIFT:
            return ">>";
        case TOKEN_RIGHT_SHIFT_ASSIGN:
            return ">>=";
        case TOKEN_RIGHT_SHIFT_LOGIC:
            return ">>>";
        case TOKEN_RIGHT_SHIFT_LOGIC_ASSIGN:
            return ">>>=";
        case TOKEN_LESS:
            return "<";
        case TOKEN_LESS_EQUAL:
            return "<=";
        case TOKEN_LEFT_SHIFT:
            return "<<";
        case TOKEN_LEFT_SHIFT_ASSIGN:
            return "<<=";
        case TOKEN_AND:
            return "&&";
        case TOKEN_AND_ASSIGN:
            return "&&=";
        case TOKEN_BIT_AND:
            return "&";
        case TOKEN_BIT_AND_ASSIGN:
            return "&=";
        case TOKEN_OR:
            return "||";
        case TOKEN_OR_ASSIGN:
            return "||=";
        case TOKEN_BIT_OR:
            return "|";
        case TOKEN_BIT_OR_ASSIGN:
            return "|=";
        case TOKEN_BIT_XOR:
            return "^";
        case TOKEN_BIT_XOR_ASSIGN:
            return "^=";
        case TOKEN_NO_INIT:
            return "---";
        case TOKEN_STRUCT:
            return "\nstruct ";
        case TOKEN_IDENTIFIER:
        case TOKEN_INTEGER:
        case TOKEN_FLOAT:
        case TOKEN_STRING:
            strlcpy(buffer, token->start, token->length + 1);
            return buffer;
        case TOKEN_EXTEND:
            return "\nextend ";
        case TOKEN_CLASS:
            return "\nclass ";
        case TOKEN_ELSE:
            return " else ";
        case TOKEN_FALSE:
            return "false ";
        case TOKEN_CONTINUE:
            return " continue ";
        case TOKEN_AT:
            return " @";
        case TOKEN_PROC:
            return "\nproc ";
        case TOKEN_FOR:
            return "\nfor ";
        case TOKEN_IMPORT:
            return "\nimport \0";
        case TOKEN_MODULE:
            return "\nmodule ";
        case TOKEN_PROGRAM:
            return "\nprogram ";
        case TOKEN_IF:
            return " if ";
        case TOKEN_NIL:
            return " nil ";
        case TOKEN_PRINT:
            return " print ";
        case TOKEN_RETURN:
            return " return ";
        case TOKEN_SUPER:
            return " super ";
        case TOKEN_THIS:
            return " this ";
        case TOKEN_TRUE:
            return " true ";
        case TOKEN_VAR:
            return " var ";
        case TOKEN_WHILE:
            return "\nwhile ";
        case TOKEN_ERROR:
            return "**ERROR!**";
        case TOKEN_EOF:
            return "EOF";
        case TOKEN_VOID:
            return " void ";
        case TOKEN_ARROW:
            return " -> ";
        case TOKEN_SWITCH:
            return " switch ";
        case TOKEN_UNTIL:
            return " until ";
        default:
            return "???";
    }
}
