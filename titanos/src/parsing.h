#pragma once

#include "common.h"
#include "parser.h"
#include "component.h"
#include "types/type.h"
#include "decl.h"

#define CONSUME_EOS_OR_EXIT if (!consume(TOKEN_EOS, "Expected ';'")) return NULL
#define CONSUME_START_BRACE_OR_EXIT if (!consume(TOKEN_LBRACE, "Expected '{'")) return NULL
#define CONSUME_END_BRACE_OR_EXIT if (!consume(TOKEN_RBRACE, "Expected '}'")) return NULL
#define UPDATE(__X) do { range_expand(&__X->span, &prev_tok); } while (0)
#define UPDATE_AND_RETURN(__X) do { UPDATE(__X); return __X; } while (0)
#define CONSUME_REQ_EOS_AND_RETURN(__X) do { UPDATE(__X); CONSUME_EOS_OR_EXIT; return __X; } while(0)

extern Parser *current_parser;
Parser *parse(Component *component, Module *module, const char *filename, bool is_interface);

bool try_consume(TokenType type);
bool consume(TokenType type, const char *message, ...);
void error_at_current(const char *message);
void error_at_prev(const char *message);
void setup_parse_rules(void);
void recover_to(TokenType type);
QualifiedType parse_type_in_range(SourceRange *range, bool public);
QualifiedType parse_type(bool public);
Token parse_possible_struct_prefix(void);

bool parse_argument_list(Decl *decl, bool public, bool type_only);
Expr *parse_expression(void);
Expr *parse_expression_block(SourceRange *range);
bool parse_struct_body(Decl *struct_decl, SourceRange body);
bool parse_enum_body(Decl *decl);
