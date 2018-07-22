//
//  lexer.c
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//

#include "lexer.h"

#include <string.h>
#include "lexer.h"
#include "common.h"


typedef struct
{
    const char *start;
    const char *current;
    int line;
} Lexer;

Lexer lexer;

static inline bool is_alphabet(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static inline bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

static inline bool is_digit_or_underscore(char c)
{
    return c == '_' || (c >= '0' && c <= '9');
}

static inline bool is_alpha(char c)
{
    return is_alphabet(c) || is_digit_or_underscore(c);
}



static inline bool reached_end(void)
{
    return *lexer.current == '\0';
}

static inline char peek_next()
{
    return lexer.current[1];
}

static inline char prev()
{
    return lexer.current[-1];
}

static inline char peek()
{
    return *lexer.current;
}

static char advance()
{
    return *(lexer.current++);
}


static Token make_token(token_type type)
{
    Token token;
    token.type = type;
    token.start = lexer.start;
    token.length = (int)(lexer.current - lexer.start);
    token.line = lexer.line;
    return token;
}


Token error_token(const char *message)
{
    Token token;
    token.line = lexer.line;
    token.start = lexer.start;
    token.length = (int)strlen(message);
    token.type = TOKEN_ERROR;
    return token;
}

static inline bool is_eol(char c)
{
    return c == '\n';
}

void skip_whitespace()
{
    while (1)
    {
        char c = peek();
        switch (c)
        {
            case '\n':
                lexer.line++;
            case ' ':
            case '\t':
            case '\r':
            case '\f':
                advance();
                break;
            case '/':
                if (peek_next() == '/')
                {
                    while (peek() != '\n') advance();
                    break;
                }
                if (peek_next() == '*')
                {
                    advance();
                    while (!reached_end())
                    {
                        advance();
                        if (peek() == '*' && peek_next() == '/')
                        {
                            lexer.current += 2;
                            break;
                        }
                    }
                    break;
                }
                return;
            default:
                return;
        }
    }
}

static inline Token scan_char()
{
    advance();
    advance();
    return make_token(TOKEN_INTEGER);
}

static inline Token scan_string()
{
    char c;
    while ((c = advance()) != '\n')
    {
        if (c == '\\' && peek() == '"')
        {
            advance();
            continue;
        }
        if (c == '\n') lexer.line++;
        if (reached_end())
        {
            return error_token("Unterminated string.");
        }
    }
    return make_token(TOKEN_STRING);
}



typedef struct
{
  const char* identifier;
  size_t length;
  token_type token;
  int next;
} Keyword;

#define ALPHABET_LEN ('z' - 'a' + 1)
static Keyword keywords[32];
static int first_test[ALPHABET_LEN];
static int keyword_counter = 0;

void register_keyword(const char *identifier, token_type type)
{
    int previous_first = first_test[identifier[0] - 'a'];
    int index = keyword_counter++;
    Keyword *keyword = &keywords[index];
    keyword->identifier = identifier;
    keyword->length = strlen(identifier);
    keyword->token = type;
    keyword->next = previous_first;
    first_test[identifier[0] - 'a'] = index;
}

static void keyword_init()
{
    if (keyword_counter > 0) return;
    for (size_t i = 0; i < ALPHABET_LEN; i++)
    {
        first_test[i] = -1;
    }
    register_keyword("extend", TOKEN_EXTEND);
    register_keyword("switch", TOKEN_SWITCH);
    register_keyword("struct", TOKEN_STRUCT);
    register_keyword("program", TOKEN_PROGRAM);
    register_keyword("proc", TOKEN_PROC);
    register_keyword("module", TOKEN_MODULE);
    register_keyword("return", TOKEN_RETURN);
    register_keyword("else", TOKEN_ELSE);
    register_keyword("while", TOKEN_WHILE);
    register_keyword("void", TOKEN_VOID);
    register_keyword("true", TOKEN_TRUE);
    register_keyword("nil", TOKEN_NIL);
    register_keyword("import", TOKEN_IMPORT);
    register_keyword("if", TOKEN_IF);
    register_keyword("false", TOKEN_FALSE);
    register_keyword("continue", TOKEN_CONTINUE);
    register_keyword("class", TOKEN_CLASS);
    register_keyword("until", TOKEN_UNTIL);

}
static inline token_type indentifier_type()
{
    int index = lexer.start[0] - 'a';
    if (index < 0) return TOKEN_IDENTIFIER;
    
    int keyword_index = first_test[index];
    while (keyword_index >= 0)
    {
        Keyword *keyword = &keywords[keyword_index];
        if ((lexer.current - lexer.start == keyword->length)
            && memcmp(lexer.start + 1, keyword->identifier + 1, keyword->length - 1) == 0)
        {
            return keyword->token;
        }
        keyword_index = keyword->next;
    }
    return TOKEN_IDENTIFIER;
}
static inline Token scan_ident()
{
    while (is_alpha(peek()))
    {
        advance();
    }
    return make_token(indentifier_type());
}

static inline bool match(char expected)
{
    if (reached_end()) return false;
    if (*lexer.current != expected) return false;
    lexer.current++;
    return true;
}

static void backtrack()
{
    lexer.current--;
}

static inline bool is_hex(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static inline bool is_hex_or_underscore(char c)
{
    return c == '_' || is_hex(c);
}

static inline bool is_oct(char c)
{
    return c >= '0' && c <= '7';
}

static inline bool is_oct_or_underscore(char c)
{
    return c == '_' || is_oct(c);
}

static inline bool is_binary(char c)
{
    return c == '0' || c == '1';
}

static inline bool is_binary_or_underscore(char c)
{
    return is_binary(c) || c == '_';
}

#define PARSE_SPECIAL_NUMBER(is_num, is_num_with_underscore, exp, EXP) \
while (is_num_with_underscore(peek())) advance(); \
bool is_float = false; \
if (peek() == '.') \
{ \
is_float = true; \
advance(); \
char c = peek(); \
if (c == '_') return error_token("Underscore may only appear between digits."); \
if (is_num(c)) advance(); \
while (is_num_with_underscore(peek())) advance(); \
} \
char c = peek(); \
if (c == exp || c == EXP) \
{ \
is_float = true; \
advance(); \
char c2 = advance(); \
if (c2 == '+' || c2 == '-') c2 = advance(); \
if (!is_num(c2)) return error_token("Invalid exponential expression"); \
while (is_digit(peek())) advance(); \
} \
if (prev() == '_') return error_token("Underscore may only appear between digits."); \
return make_token(is_float ? TOKEN_FLOAT : TOKEN_INTEGER);


static inline Token scan_hex(void)
{
    advance(); // skip the x
    if (!is_hex(advance())) return error_token("Invalid hex sequence");
    PARSE_SPECIAL_NUMBER(is_hex, is_hex_or_underscore, 'p', 'P');
}


static Token scan_oct(void)
{
    advance(); // Skip the o
    if (!is_oct(advance())) return error_token("Invalid octal sequence");
    while (is_oct_or_underscore(peek()))advance();
    return make_token(TOKEN_INTEGER);;
}


Token scan_binary(void)
{
    advance(); // Skip the b
    if (!is_binary(advance())) return error_token("Invalid binary sequence");
    while (is_binary_or_underscore(peek()))advance();
    return make_token(TOKEN_INTEGER);;
}

static inline Token scan_digit()
{
    if (prev() == '0')
    {
        switch (peek())
        {
                // case 'X': Let's not support this? REVISIT
            case 'x':
                return scan_hex();
            case 'o':
                return scan_oct();
            case 'b':
                return scan_binary();
            default:
                break;
        }
    }
    PARSE_SPECIAL_NUMBER(is_digit, is_digit_or_underscore, 'e', 'E');
}


#undef PARSE_SPECIAL_NUMBER

Token scan_token(void)
{
    skip_whitespace();
    
    lexer.start = lexer.current;
    
    if (reached_end()) return make_token(TOKEN_EOF);
    
    char c = advance();
    
    switch (c)
    {
        case '@':
            return make_token(TOKEN_AT);
        case '\'':
            return scan_char();
        case '"':
            return scan_string();
        case '#':
            return make_token(TOKEN_HASH);
        case '$':
            return make_token(TOKEN_DOLLAR);
        case ',':
            return make_token(TOKEN_COMMA);
        case ';':
            return make_token(TOKEN_EOS);
        case '{':
            return make_token(TOKEN_BRACE_L);
        case '}':
            return make_token(TOKEN_BRACE_R);
        case '(':
            return make_token(TOKEN_PAREN_L);
        case ')':
            return make_token(TOKEN_PAREN_R);
        case '[':
            return make_token(TOKEN_BRACKET_L);
        case ']':
            return make_token(TOKEN_BRACKET_R);
        case '.':
            return make_token(TOKEN_DOT);
        case '~':
            return make_token(TOKEN_BIT_NOT);
        case ':':
            if (match(':')) return make_token(TOKEN_COLCOLON);
            return make_token(match('=') ? TOKEN_COLON_ASSIGN : TOKEN_COLON);
        case '!':
            return make_token(match('=') ? TOKEN_NOT_EQUAL : TOKEN_NOT);
        case '/':
            return make_token(match('=') ? TOKEN_DIV_ASSIGN : TOKEN_DIV);
        case '*':
            return make_token(match('=') ? TOKEN_MULT_ASSIGN : TOKEN_MULT);
        case '=':
            return make_token(match('=') ? TOKEN_EQUAL : TOKEN_ASSIGN);
        case '^':
            if (match('^')) return make_token(TOKEN_POW);
            return make_token(match('=') ? TOKEN_BIT_XOR_ASSIGN : TOKEN_BIT_XOR);
        case '?':
            return make_token(match(':') ? TOKEN_ELVIS : TOKEN_QUESTION);
        case '<':
            if (match('<')) return make_token(match('=') ? TOKEN_LEFT_SHIFT_ASSIGN : TOKEN_LEFT_SHIFT);
            return make_token(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>':
            if (match('>'))
            {
                if (match('>'))
                {
                    return make_token(match('=') ? TOKEN_RIGHT_SHIFT_LOGIC_ASSIGN : TOKEN_RIGHT_SHIFT_LOGIC);
                }
                return make_token(match('=') ? TOKEN_RIGHT_SHIFT_ASSIGN : TOKEN_RIGHT_SHIFT);
            }
            return make_token(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '&':
            if (match('&'))
            {
                return make_token(match('=') ? TOKEN_AND_ASSIGN : TOKEN_AND);
            }
            return make_token(match('=') ? TOKEN_BIT_AND_ASSIGN : TOKEN_BIT_AND);
        case '|':
            if (match('|'))
            {
                return make_token(match('=') ? TOKEN_OR_ASSIGN : TOKEN_OR);
            }
            return make_token(match('=') ? TOKEN_BIT_OR_ASSIGN : TOKEN_BIT_OR);
        case '+':
            if (match('+')) return make_token(TOKEN_PLUSPLUS);
            if (match('=')) return make_token(TOKEN_PLUS_ASSIGN);
            return make_token(TOKEN_PLUS);
        case '-':
            if (match('>')) return make_token(TOKEN_ARROW);
            if (match('-'))
            {
                return make_token(match('-') ? TOKEN_NO_INIT : TOKEN_MINUSMINUS);
            }
            if (match('=')) return make_token(TOKEN_MINUS_ASSIGN);
            return make_token(TOKEN_MINUS);
            
        default:
            if (is_digit(c)) return scan_digit();
            if (is_alphabet(c)) return scan_ident();
            return error_token("Unexpected character.");
    }
    
}

void init_lexer(const char *source)
{
    keyword_init();
    lexer.start = source;
    lexer.current = source;
    lexer.line = 1;
}
