#pragma once
//
// Created by Christoffer Lerno on 2018-12-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "lexer.h"
#include "common.h"
#include "vector.h"
#include "component.h"
#include "value.h"

typedef struct _Ast Ast;
typedef struct _Type Type;
typedef enum _AstType
{
    AST_ATTRIBUTE,
    AST_COMPOUND_STMT,
    AST_IF_STMT,
    AST_WHILE_STMT,
    AST_DO_STMT,
    AST_DECLARE_STMT,
    AST_DEFER_STMT,
    AST_SWITCH_STMT,
    AST_CASE_STMT,
    AST_DEFAULT_STMT,
    AST_BREAK_STMT,
    AST_CONTINUE_STMT,
    AST_RETURN_STMT,
    AST_GOTO_STMT,
    AST_FOR_STMT,
    AST_LABEL,
    AST_EXPR_STMT,
    AST_DEFER_RELASE,
    AST_ASM_STMT,
} AstType;

typedef struct _Expr Expr;

typedef struct _DeferList
{
    Ast *defer_start;
    Ast *defer_end;
} DeferList;


typedef struct _AstDeclareStmt
{
    struct _Decl *decl;
} AstDeclareStmt;



typedef struct _AstCompoundStmt
{
    Vector *stmts; // May be NULL!
    DeferList defer_list;
} AstCompoundStmt;


typedef struct _AstIfStmt
{
    Expr *expr;
    Ast *then_body;
    Ast *else_body;
} AstIfStmt;

typedef struct _AstDefaultStmt
{
    Vector *stmts;
    DeferList defer_list;
} AstDefaultStmt;

typedef struct _AstCaseStmt
{
    Expr *expr;
    Vector *stmts;
    DeferList defer_list;
} AstCaseStmt;

typedef struct _AstForStmt
{
    Ast *init;
    Expr *cond;
    Expr *incr;
    Ast *body;
} AstForStmt;

struct _AstDoWhileStmt
{
    Expr *expr;
    Ast *body;
};

typedef struct _AstDoWhileStmt AstDoStmt;
typedef struct _AstDoWhileStmt AstWhileStmt;

typedef struct _AstSwitchStmt
{
    Expr *expr;
    Vector *case_list;
    Ast *default_stmt;
} AstSwitchStmt;


typedef struct _Type Type;

typedef struct _AstFuncDefinition
{
    bool is_public : 1;
    bool is_exported : 1;
    Type *func_decl;
    Ast *body; // AstCompoundStmt will be NULL in interfaces.
    Vector *defers; // NULL unless defers
} AstFuncDefinition;

typedef struct _AstGotoStmt
{
    union
    {
        Expr *label;
        Ast *label_stmt;
    };
    DeferList defer_list;

} AstGotoStmt;

typedef struct _AstReturnStmt
{
    Expr *expr; // May be NULL
    Ast *defer_top;
} AstReturnStmt;

typedef struct _AstDeferStmt
{
    bool emit_boolean : 1;
    Ast *body;
    Ast *prev_defer;
} AstDeferStmt;



typedef struct _AstLabel
{
    bool is_used : 1;
    Token label_name;
    Ast *defer_top;
} AstLabelStmt;


typedef struct _AstAttribute
{
    Token name;
    Expr *value;
} AstAttribute;



typedef struct _AstDeferReleaseStmt
{
    Ast *inner;
    DeferList list;
} AstDeferReleaseStmt;



typedef struct _AstExprStmt
{
    Expr *expr;
} AstExprStmt;

typedef struct _Ast
{
    AstType ast_id : 8;
    Token span;
    union {

        AstAttribute attribute;
        AstDeclareStmt declare_stmt;
        AstCompoundStmt compound_stmt;
        AstIfStmt if_stmt;
        AstDoStmt do_stmt;
        AstWhileStmt while_stmt;
        AstDeferStmt defer_stmt;
        AstSwitchStmt switch_stmt;
        AstCaseStmt case_stmt;
        AstDefaultStmt default_stmt;
        AstGotoStmt goto_stmt;
        AstReturnStmt return_stmt;
        AstForStmt for_stmt;
        AstLabelStmt label_stmt;
        AstDeferReleaseStmt defer_release_stmt;
        AstExprStmt expr_stmt;
    };
} Ast;

void print_ast(Ast *ast, unsigned current_indent);
void print_sub_ast(const char *header, unsigned current_indent, Ast *ast);
Ast *new_ast(AstType type);
Ast *new_ast_with_span(AstType type, Token *span);
Ast *end_ast(Ast *ast, Token *end);

Ast *ast_compound_stmt_last(Ast *compound_stmt);