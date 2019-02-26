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
typedef struct _Decl Decl;

typedef struct _Label
{
    Ast *label_stmt;
    const char *name;
    Ast *first_goto;
    Ast *defer;
} Label;

typedef enum _AstType
{
    AST_ATTRIBUTE,
    AST_COMPOUND_STMT,
    AST_IF_STMT,
    AST_WHILE_STMT,
    AST_DO_STMT,
    AST_COND_STMT,
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

typedef enum CondType
{
    COND_DECL,
    COND_EXPR,
} CondType;

typedef struct _AstCondStmt
{
    CondType cond_type : 2;
    union {
        Decl *decl;
        Expr *expr;
    };
} AstCondStmt;

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
    Ast *cond;
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

typedef struct _AstWhileStmt
{
    Ast *cond;
    Ast *body;
} AstWhileStmt;

typedef struct _AstDoStmt
{
    Expr *expr;
    Ast *body;
} AstDoStmt;


typedef struct _AstSwitchStmt
{
    Ast *cond;
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

typedef enum _GotoType
{
    GOTO_NOT_ANALYSED,
    GOTO_JUMP_FORWARD,
    GOTO_JUMP_BACK
} GotoType;

typedef struct _AstGotoStmt
{
    GotoType type : 1;
    union
    {
        const char *label_name;
        Label *label;
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
    Ast *body; // Compound statement
    Ast *prev_defer;
} AstDeferStmt;



typedef struct _AstLabel
{
    bool is_used : 1;
    bool in_defer : 1;
    const char *label_name;
    Ast *defer;
} AstLabelStmt;


typedef struct _AstAttribute
{
    const char *name;
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

// Ordering here is in priority if two branches should have the same exit.
typedef enum _ExitType
{
    EXIT_NONE,
    EXIT_BREAK,
    EXIT_CONTINUE,
    EXIT_RETURN
} ExitType;


typedef struct _Ast
{
    AstType ast_id : 8;
    ExitType exit : 3;
    SourceRange span;
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
        AstCondStmt cond_stmt;
    };
} Ast;

void print_ast(Ast *ast, unsigned current_indent);
void print_sub_ast(const char *header, unsigned current_indent, Ast *ast);
Ast *new_ast(AstType type);
Ast *new_ast_with_span(AstType type, SourceRange span);

typedef enum _CondValue
{
    COND_VARIABLE,
    COND_TRUE,
    COND_FALSE
} CondValue;

/**
 * Check if condition is always true
 * @param cond the condition to test
 * @return COND_VARIABLE if non constant, othersie COND_TRUE / COND_FALSE
 */
CondValue ast_cond_value(Ast *cond_stmt);

Ast *ast_compound_stmt_last(Ast *compound_stmt);

