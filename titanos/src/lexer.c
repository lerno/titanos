//
//  lexer.c
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//

#include "lexer.h"
#include "array.h"
#include "error.h"
#include "diagnostics.h"
#include "table.h"
#include "symtab.h"

#include <string.h>

#define MAX_FILES (0xFFFF - 1)
#define MAX_LINE (0xFFFFFF - 1)

const SourceLoc INVALID_LOC = { .id = UINT32_MAX };

Array files;
File pseudo_file;

Token tok;
Token prev_tok;

typedef struct
{
    const char *begin;
    const char *start;
    const char *current;
    uint16_t source_file;
    LexerState lexer_state;
    File *current_file;
    Token saved_tok;
    Token saved_prev_tok;
    SourceLoc last_in_range;
} Lexer;


#define LEXER_STACK_MAX 256

static Lexer lexer_stack[LEXER_STACK_MAX];
static int lexer_pointer = 0;
static Lexer lexer;


const char *token_type_to_string(TokenType type)
{
    switch (type)
    {
        case TOKEN_LPAREN:
            return "(";
        case TOKEN_RPAREN:
            return ")";
        case TOKEN_LBRACE:
            return "{";
        case TOKEN_RBRACE:
            return "}";
        case TOKEN_LBRACKET:
            return "[";
        case TOKEN_RBRACKET:
            return "]";
        case TOKEN_COMMA:
            return ",";
        case TOKEN_HASH:
            return "#";
        case TOKEN_DOLLAR:
            return "$";
        case TOKEN_DOT:
            return ".";
        case TOKEN_EOS:
            return ";";
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
        case TOKEN_STAR:
            return "*";
        case TOKEN_MULT_ASSIGN:
            return "*=";
        case TOKEN_POW:
            return "^^";
        case TOKEN_MOD:
            return "%";
        case TOKEN_MOD_ASSIGN:
            return "%=";
        case TOKEN_DIV:
            return "/";
        case TOKEN_DIV_ASSIGN:
            return "/=";
        case TOKEN_NOT_EQUAL:
            return "!=";
        case TOKEN_EQ:
            return "=";
        case TOKEN_EQEQ:
            return "==";
        case TOKEN_COLON:
            return ":";
        case TOKEN_COLON_ASSIGN:
            return ":=";
        case TOKEN_COLCOLON:
            return "::";
        case TOKEN_DOTDOT:
            return "..";
        case TOKEN_ELIPSIS:
            return "...";
        case TOKEN_GREATER:
            return ">";
        case TOKEN_GREATER_EQ:
            return ">=";
        case TOKEN_RIGHT_SHIFT:
            return ">>";
        case TOKEN_RIGHT_SHIFT_ASSIGN:
            return ">>=";
        case TOKEN_LESS:
            return "<";
        case TOKEN_LESS_EQ:
            return "<=";
        case TOKEN_LEFT_SHIFT:
            return "<<";
        case TOKEN_LEFT_SHIFT_ASSIGN:
            return "<<=";
        case TOKEN_ARROW:
            return "->";
        case TOKEN_AND:
            return "&&";
        case TOKEN_AND_ASSIGN:
            return "&&=";
        case TOKEN_AMP:
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
        case TOKEN_IDENTIFIER:
            return "<identifier>";
        case TOKEN_STRING:
            return "<string>";
        case TOKEN_INTEGER:
            return "<int>";
        case TOKEN_FLOAT:
            return "<float>";
        case TOKEN_QUESTION:
            return "?";
        case TOKEN_ELVIS:
            return "?:";
        case TOKEN_VOID:
            return "void";
        case TOKEN_ALIAS:
            return "alias";
        case TOKEN_CONST:
            return "const";
        case TOKEN_VOLATILE:
            return "volatile";
        case TOKEN_ELSE:
            return "else";
        case TOKEN_FALSE:
            return "false";
        case TOKEN_CONTINUE:
            return "continue";
        case TOKEN_FUNC:
            return "func";
        case TOKEN_FOR:
            return "for";
        case TOKEN_IMPORT:
            return "import";
        case TOKEN_MODULE:
            return "module";
        case TOKEN_IF:
            return "if";
        case TOKEN_NIL:
            return "nil";
        case TOKEN_RETURN:
            return "return";
        case TOKEN_GOTO:
            return "goto";
        case TOKEN_DEFER:
            return "defer";
        case TOKEN_TRUE:
            return "true";
        case TOKEN_WHILE:
            return "while";
        case TOKEN_CASE:
            return "case";
        case TOKEN_ASM:
            return "asm";
        case TOKEN_DEFAULT:
            return "default";
        case TOKEN_SWITCH:
            return "switch";
        case TOKEN_UNTIL:
            return "until";
        case TOKEN_BREAK:
            return "break";
        case TOKEN_TYPE:
            return "type";
        case TOKEN_DO:
            return "do";
        case TOKEN_PUBLIC:
            return "public";
        case TOKEN_LOCAL:
            return "local";
        case TOKEN_STRUCT:
            return "struct";
        case TOKEN_UNION:
            return "union";
        case TOKEN_ENUM:
            return "enum";
        case TOKEN_AT:
            return "@";
        case TOKEN_AS:
            return "as";
        case TOKEN_ERROR:
            return "<error>";
        case TOKEN_EOF:
            return "<eof>";
        case TOKEN_CAST:
            return "cast";
        case TOKEN_SIZEOF:
            return "sizeof";
        case TOKEN_MACRO:
            return "macro";
        case TOKEN_F256:
            return "f256";
        case TOKEN_I256:
            return "i256";
        case TOKEN_U256:
            return "u256";
        case TOKEN_F128:
            return "f128";
        case TOKEN_I128:
            return "i128";
        case TOKEN_U128:
            return "u128";
        case TOKEN_F64:
            return "f64";
        case TOKEN_I64:
            return "i64";
        case TOKEN_U64:
            return "u64";
        case TOKEN_F32:
            return "f32";
        case TOKEN_I32:
            return "i32";
        case TOKEN_U32:
            return "u32";
        case TOKEN_F16:
            return "f16";
        case TOKEN_I16:
            return "i16";
        case TOKEN_U16:
            return "u16";
        case TOKEN_I8:
            return "i8";
        case TOKEN_U8:
            return "u8";
        case TOKEN_BOOL:
            return "bool";
    }
    UNREACHABLE
}

static inline bool is_lower_case(char c)
{
	return c >= 'a' && c <= 'z';
}

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

static inline TokenType check_keyword(const char *keyword, TokenType type)
{
    size_t len = lexer.current - lexer.start;
    if (memcmp(lexer.start + 1, keyword + 1, len - 1) == 0)
    {
        return type;
    }
    return TOKEN_IDENTIFIER;
}

static inline TokenType check_two_keywords(const char *keyword_1, const char *keyword_2, int diff_pos,
        TokenType type_1, TokenType type_2)
{
    size_t len = lexer.current - lexer.start;
    if (lexer.start[diff_pos] == keyword_1[diff_pos])
    {
        if (memcmp(lexer.start + 1, keyword_1 + 1, len - 1) == 0) return type_1;
    }
    else if (lexer.start[diff_pos] == keyword_2[diff_pos])
    {
        if (memcmp(lexer.start + 1, keyword_2 + 1, len - 1) == 0) return type_2;
    }
    return TOKEN_IDENTIFIER;
}

#define HASH(first, len) (len + (((int)(first - 'a')) << 3))
#define HASH2(first, second) (first + (second << 8))
// Yes this is an ugly hand written keyword identifier. It should be benchmarked against
// an table based state machine.
static inline TokenType identifier_type()
{
    int len = (int) (lexer.current - lexer.start);
	char current_value = lexer.start[0];
    if (len > 8 || !is_lower_case(current_value)) return TOKEN_IDENTIFIER;
    switch (HASH(current_value, len))
    {
        case HASH('a', 2):
            return check_keyword("as", TOKEN_AS);
        case HASH('a', 3):
            return check_keyword("asm", TOKEN_ASM);
        case HASH('a', 5):
            return check_keyword("alias", TOKEN_ALIAS);
        case HASH('b', 4):
            return check_keyword("bool", TOKEN_BOOL);
        case HASH('b', 5):
            return check_keyword("break", TOKEN_BREAK);
        case HASH('c', 4):
            return check_two_keywords("cast", "case", 3, TOKEN_CAST, TOKEN_CASE);
        case HASH('c', 5):
            return check_keyword("const", TOKEN_CONST);
        case HASH('c', 8):
            return check_keyword("continue", TOKEN_CONTINUE);
        case HASH('d', 2):
            return check_keyword("do", TOKEN_DO);
        case HASH('d', 5):
            return check_keyword("defer", TOKEN_DEFER);
        case HASH('d', 7):
            return check_keyword("default", TOKEN_DEFAULT);
        case HASH('e', 4):
            return check_two_keywords("else", "enum", 1, TOKEN_ELSE, TOKEN_ENUM);
        case HASH('f', 3):
            switch (HASH2(lexer.start[1], lexer.start[2]))
            {
                case HASH2('1', '6'):
                    return TOKEN_F16;
                case HASH2('3', '2'):
                    return TOKEN_F32;
                case HASH2('6', '4'):
                    return TOKEN_F64;
                case HASH2('o', 'r'):
                    return TOKEN_FOR;
                default:
                    return TOKEN_IDENTIFIER;
            }
        case HASH('f', 4):
            switch (lexer.start[1])
            {
                case '1':
                    return check_keyword("f128", TOKEN_F128);
                case '2':
                    return check_keyword("f256", TOKEN_F256);
                default:
                    return check_keyword("func", TOKEN_FUNC);
            }
        case HASH('f', 5):
            return check_keyword("false", TOKEN_FALSE);
        case HASH('g', 4):
            return check_keyword("goto", TOKEN_GOTO);
        case HASH('i', 2):
            switch (lexer.start[1])
            {
                case '8':
                    return TOKEN_I8;
                case 'f':
                    return TOKEN_IF;
                default:
                    return TOKEN_IDENTIFIER;
            }
        case HASH('i', 3):
            switch (HASH2(lexer.start[1], lexer.start[2]))
            {
                case HASH2('1', '6'):
                    return TOKEN_I16;
                case HASH2('3', '2'):
                    return TOKEN_I32;
                case HASH2('6', '4'):
                    return TOKEN_I64;
                default:
                    return TOKEN_IDENTIFIER;
            }
        case HASH('i', 4):
            return check_two_keywords("i128", "i256", 2, TOKEN_I128, TOKEN_I256);
        case HASH('i', 6):
            return check_keyword("import", TOKEN_IMPORT);
        case HASH('l', 5):
            return check_keyword("local", TOKEN_LOCAL);
        case HASH('m', 5):
            return check_keyword("macro", TOKEN_MACRO);
        case HASH('m', 6):
            return check_keyword("module", TOKEN_MODULE);
        case HASH('n', 3):
            return check_keyword("nil", TOKEN_NIL);
        case HASH('p', 6):
            return check_keyword("public", TOKEN_PUBLIC);
        case HASH('r', 6):
            return check_keyword("return", TOKEN_RETURN);
        case HASH('s', 6):
            switch (lexer.start[1])
            {
                case 't':
                    return check_keyword("struct", TOKEN_STRUCT);
                case 'i':
                    return check_keyword("sizeof", TOKEN_SIZEOF);
                case 'w':
                    return check_keyword("switch", TOKEN_SWITCH);
                default:
                    return TOKEN_IDENTIFIER;
            }
        case HASH('t', 4):
            return check_two_keywords("true", "type", 1, TOKEN_TRUE, TOKEN_TYPE);
        case HASH('u', 2):
            return lexer.start[1] == '8' ? TOKEN_U8 : TOKEN_IDENTIFIER;
        case HASH('u', 3):
            switch (HASH2(lexer.start[1], lexer.start[2]))
            {
                case HASH2('1', '6'):
                    return TOKEN_U16;
                case HASH2('3', '2'):
                    return TOKEN_U32;
                case HASH2('6', '4'):
                    return TOKEN_U64;
                default:
                    return TOKEN_IDENTIFIER;
            }
        case HASH('u', 4):
            return check_two_keywords("u128", "u256", 2, TOKEN_U128, TOKEN_U256);
        case HASH('u', 5):
            return check_two_keywords("union", "until", 2, TOKEN_UNION, TOKEN_UNTIL);
        case HASH('v', 4):
            return check_keyword("void", TOKEN_VOID);
        case HASH('v', 8):
            return check_keyword("volatile", TOKEN_VOLATILE);
        case HASH('w', 5):
            return check_keyword("while", TOKEN_WHILE);
        default:
            return TOKEN_IDENTIFIER;
    }
}

#undef HASH

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

static char next()
{
    return *(lexer.current++);
}


Token error_token(const char *message)
{
    Token token;
    token.type = TOKEN_ERROR;
    token.start = lexer.start;
    token.span.length = 1;
    token.span.loc.id = (uint32_t) (lexer.current_file->start.id + (lexer.begin - lexer.start));
    error_at(token.span, message);
    return token;
}

static Token make_token(TokenType type)
{
    size_t token_size = lexer.current - lexer.start;
    if (token_size > TOKEN_MAX_LENGTH) return error_token("Token exceeding max length");
    Token token;
    token.type = type;
    token.start = lexer.start;
    token.span = (SourceRange)
            { .loc.id = (uint32_t) (lexer.current_file->start.id + (lexer.start - lexer.begin)),
                    .length = (uint16_t) token_size };
    return token;
}


/**
 * Skip the comments. Consider parsing docs.
 *
 * @return true if we skipped correctly, false if we got caught in a comment.
 */
bool skip_whitespace()
{
    while (1)
    {
        char c = peek();
        switch (c)
        {
            case '\n':
            case ' ':
            case '\t':
            case '\r':
            case '\f':
                next();
                break;
            case '/':
                if (peek_next() == '/')
                {
                    while (!reached_end() && peek() != '\n') next();
                    break;
                }
                if (peek_next() == '*')
                {
                    while (1)
                    {
                        next();
                        if (reached_end()) return false;
                        if (peek() == '*' && peek_next() == '/')
                        {
                            lexer.current += 2;
                            break;
                        }
                    }
                    break;
                }
                if (peek_next() == '+')
                {
                    int nesting_depth = 1;
                    while (1)
                    {
                        next();
                        if (reached_end()) return false;
                        if (peek() == '/' && peek_next() == '+')
                        {
                            lexer.current += 2;
                            nesting_depth++;
                            continue;
                        }
                        if (peek() == '+' && peek_next() == '/')
                        {
                            lexer.current += 2;
                            if (--nesting_depth == 0) break;
                        }
                    }
                    break;
                }
                return true;
            default:
                return true;
        }
    }
}

static inline Token scan_char()
{
    next();
    next();
    return make_token(TOKEN_INTEGER);
}

static inline Token scan_string()
{
    char c;

    while ((c = next()) != '"')
    {
        if (c == '\\' && peek() == '"')
        {
            next();
            continue;
        }
        if (reached_end())
        {
            return error_token("Unterminated string.");
        }
    }
    return make_token(TOKEN_STRING);
}

static inline Token scan_ident()
{
    while (is_alpha(peek()))
    {
        next();
    }
    if (lexer.lexer_state == LEXER_STATE_IGNORE_KEYWORDS)
    {
        return make_token(TOKEN_IDENTIFIER);
    }
    TokenType type = identifier_type();
    Token token = make_token(type);
    if (type == TOKEN_IDENTIFIER)
    {
        uint32_t len = (uint32_t) (lexer.current - lexer.start);
        token.string = symtab_add(lexer.start, len);
    }
    return token;
}

static inline bool match(char expected)
{
    if (reached_end()) return false;
    if (*lexer.current != expected) return false;
    lexer.current++;
    return true;
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
while (is_num_with_underscore(peek())) next(); \
bool is_float = false; \
if (peek() == '.') \
{ \
is_float = true; \
next(); \
char c = peek(); \
if (c == '_') return error_token("Underscore may only appear between digits."); \
if (is_num(c)) next(); \
while (is_num_with_underscore(peek())) next(); \
} \
char c = peek(); \
if (c == exp || c == EXP) \
{ \
is_float = true; \
next(); \
char c2 = next(); \
if (c2 == '+' || c2 == '-') c2 = next(); \
if (!is_num(c2)) return error_token("Invalid exponential expression"); \
while (is_digit(peek())) next(); \
} \
if (prev() == '_') return error_token("Underscore may only appear between digits."); \
return make_token(is_float ? TOKEN_FLOAT : TOKEN_INTEGER);


static inline Token scan_hex(void)
{
    next(); // skip the x
    if (!is_hex(next())) return error_token("Invalid hex sequence");
    PARSE_SPECIAL_NUMBER(is_hex, is_hex_or_underscore, 'p', 'P');
}

void push_lexer(SourceRange *range)
{
    if (lexer_pointer == LEXER_STACK_MAX)
    {
        PRINT_ERROR("Reached max lexer stack depth %d", LEXER_STACK_MAX);
        exit(-1);
    }
    lexer.saved_prev_tok = prev_tok;
    lexer.saved_tok = tok;
    lexer_stack[lexer_pointer++] = lexer;
    DEBUG_LOG("Pushing lexer state %d", lexer_pointer);
    lexer.current_file = source_get_file(range->loc);
    lexer.begin = lexer.current_file->contents;
    lexer.start = lexer.begin + range->loc.id - lexer.current_file->start.id;
    lexer.current = lexer.start;
    lexer.lexer_state = LEXER_STATE_NORMAL;
    lexer.last_in_range.id = range->loc.id + range->length;
    advance();
}

Token pop_lexer(void)
{
    DEBUG_LOG("Popping lexer state %d", lexer_pointer);
    Token token;
    if (lexer.last_in_range.id > tok.span.loc.id)
    {
        token = tok;
    }
    else
    {
        token = (Token) { .type = TOKEN_EOF };
    }
    if (lexer_pointer < 1)
    {
        FATAL_ERROR("Popped last lexer");
    }
    lexer = lexer_stack[--lexer_pointer];
    prev_tok = lexer.saved_prev_tok;
    tok = lexer.saved_tok;
    return token;
}

static Token scan_oct(void)
{
    next(); // Skip the o
    if (!is_oct(next())) return error_token("Invalid octal sequence");
    while (is_oct_or_underscore(peek())) next();
    return make_token(TOKEN_INTEGER);;
}


Token scan_binary(void)
{
    next(); // Skip the b
    if (!is_binary(next())) return error_token("Invalid binary sequence");
    while (is_binary_or_underscore(peek())) next();
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

const char *skip_to_end_of_previous_line(const char *file_start, const char *start)
{
    start--;
    if (start < file_start) return file_start;
    while (start > file_start)
    {
        switch (start[0])
        {
            case '\n':
            case '\r':
            case '\f':
                start--;
                continue;
            default:
                return start;
        }
    }
    return start;
}

const char *source_line_start(const char *file_start, const char *start)
{
    if (start < file_start) return file_start;
    while (start > file_start && start[0] != '\n')
    {
        start--;
    }
    while (1)
    {
        switch (start[0])
        {
            case '\n':
            case '\r':
            case '\f':
                start++;
                continue;
            default:
                return start;
        }
    }
}



const char *find_line_end(const char *begin)
{
    const char *start = begin;
    while (start[0] != '\0' && start[0] != '\n')
    {
        start++;
    }
    while (start > begin)
    {
        switch (start[0])
        {
            case '\n':
            case ' ':
            case '\t':
            case '\r':
            case '\f':
                --start;
                continue;
            default:
                break;
        }
        break;
    }
    return start + 1;
}

#undef PARSE_SPECIAL_NUMBER

Token lookahead(int steps)
{
    assert(steps > 0 && "Lookahead cannot be 0 or less");
    const char *current = lexer.current;
    const char *begin = lexer.begin;
    for (unsigned i = 0; i < steps - 1; i++)
    {
        scan_token();
    }
    Token token = scan_token();
    lexer.current = current;
    lexer.begin = begin;
    return token;
}

void set_lexer_state(LexerState state)
{
    assert(state != lexer.lexer_state);
    lexer.lexer_state = state;
}

Token scan_token(void)
{
    skip_whitespace();
    
    lexer.start = lexer.current;
    
    if (reached_end()) return make_token(TOKEN_EOF);
    
    char c = next();
    
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
            return make_token(TOKEN_LBRACE);
        case '}':
            return make_token(TOKEN_RBRACE);
        case '(':
            return make_token(TOKEN_LPAREN);
        case ')':
            return make_token(TOKEN_RPAREN);
        case '[':
            return make_token(TOKEN_LBRACKET);
        case ']':
            return make_token(TOKEN_RBRACKET);
        case '.':
            if (match('.')) return make_token(match('.') ? TOKEN_ELIPSIS : TOKEN_DOTDOT);
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
            return make_token(match('=') ? TOKEN_MULT_ASSIGN : TOKEN_STAR);
        case '=':
            return make_token(match('=') ? TOKEN_EQEQ : TOKEN_EQ);
        case '^':
            if (match('^')) return make_token(TOKEN_POW);
            return make_token(match('=') ? TOKEN_BIT_XOR_ASSIGN : TOKEN_BIT_XOR);
        case '?':
            return make_token(match(':') ? TOKEN_ELVIS : TOKEN_QUESTION);
        case '<':
            if (match('<')) return make_token(match('=') ? TOKEN_LEFT_SHIFT_ASSIGN : TOKEN_LEFT_SHIFT);
            return make_token(match('=') ? TOKEN_LESS_EQ : TOKEN_LESS);
        case '>':
            if (match('>')) return make_token(match('=') ? TOKEN_RIGHT_SHIFT_ASSIGN : TOKEN_RIGHT_SHIFT);
            return make_token(match('=') ? TOKEN_GREATER_EQ : TOKEN_GREATER);
        case '%':
            return make_token(match('=') ? TOKEN_MOD_ASSIGN : TOKEN_MOD);
        case '&':
            if (match('&'))
            {
                return make_token(match('=') ? TOKEN_AND_ASSIGN : TOKEN_AND);
            }
            return make_token(match('=') ? TOKEN_BIT_AND_ASSIGN : TOKEN_AMP);
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
            if (c == '_' || is_alphabet(c)) return scan_ident();
            return error_token("Unexpected character.");
    }
}

void advance(void)
{
    prev_tok = tok;

    while (1)
    {
        tok = scan_token();
        // printf(">>> %.*s => %s\n", tok.length, tok.start, token_type_to_string(tok.type));
        if (tok.type != TOKEN_ERROR) break;
    }
    if (tok.type != TOKEN_EOF && lexer.last_in_range.id > 0 && tok.span.loc.id > lexer.last_in_range.id)
    {
        tok.type = TOKEN_ERROR;
    }
}



void init_lexer(const char *filename, const char *source, size_t size)
{
    static bool files_initialized = false;
    if (!files_initialized)
    {
        array_init(&files);
        files_initialized = true;
    }
    if (files.count > MAX_FILES)
    {
        PRINT_ERROR("Exceeded max number of files %d", MAX_FILES);
    }
    lexer.current_file = malloc(sizeof(File));
    lexer.current_file->contents = source;
    lexer.current_file->name = filename;
    lexer.last_in_range.id = 0;
    lexer.current_file->start.id = files.count == 0 ? 0 : ((File *)files.entries[files.count - 1])->end.id;
    assert(lexer.current_file->start.id + size < UINT32_MAX);
    lexer.current_file->end.id = (unsigned)(lexer.current_file->start.id + size);
    array_add(&files, lexer.current_file);
    lexer.begin = source;
    lexer.start = source;
    lexer.current = source;
}

File *source_get_file(SourceLoc loc)
{
    if (loc.id == INVALID_LOC.id)
    {
        pseudo_file.contents = "---";
        return &pseudo_file;
    }
    if (lexer.current_file->start.id <= loc.id) return lexer.current_file;
    unsigned low = 0;
    unsigned high = files.count - 2;
    while (1)
    {
        unsigned mid = (high + low) / 2;
        File *file = files.entries[mid];
        if (file->start.id > loc.id)
        {
            high = mid - 1;
            continue;
        }
        if (file->end.id < loc.id)
        {
            low = mid + 1;
            continue;
        }
        return file;
    }
}

bool token_compare_str(const Token *token1, const char *string)
{
    size_t len = strlen(string);
    if (token1->span.length != len) return false;
    return memcmp(token1->start, string, len) == 0;
}

void range_expand(SourceRange *to_update, Token *end_token)
{
    if (to_update->length == 0)
    {
        *to_update = end_token->span;
    }
    else
    {
        to_update->length = (uint16_t)(end_token->span.loc.id + end_token->span.length - to_update->loc.id);
    }
}

Token token_wrap(const char *string)
{
    return (Token) { .start = string, .span = { .loc = INVALID_LOC, .length = (uint16_t) strlen(string) }, .type = TOKEN_IDENTIFIER};
}

