#pragma once
//
// Created by Christoffer Lernö on 2019-03-05.
//


#include "decl.h"

typedef struct _Component Component;
typedef struct _Module Module;

void init_semantic_analyser(Component *component, Module *module, const char *filename, bool is_interface);
void sema_add_module(SourceRange start, Token module_name);
void sema_add_alias(bool is_public, SourceLoc loc, Token *name, SourceRange unparsed_type);
void sema_add_enum(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_type,
                   SourceRange unparsed_body);
void sema_add_struct(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_body);
void sema_add_union(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_body);
void sema_add_func_type(bool is_public, SourceLoc loc, Token *name, SourceRange unparsed_type);
void sema_add_import(SourceRange start, Token import, Token alias, ImportType type);
void sema_add_func_definition(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_type,
                              SourceRange unparsed_parameters, SourceRange unparsed_body, bool has_body);
void sema_add_var_definition(bool is_public, SourceLoc loc, Token *name, SourceRange unparsed_type, Vector *attributes, SourceRange unparsed_init);
void sema_add_array_entry(Token name, SourceRange range);
Expr *sema_expr_const_int(SourceRange span, Value value);
Expr *sema_expr_binary(TokenType type, Expr *left_side, Expr *right_side);

QualifiedType sema_resolve_prefixed_type(Token module, Token type_name);
QualifiedType sema_resolve_type(Token type_name, bool public_use);
Expr *sema_create_identifier_expr(Token token);
void sema_analyse(void);
