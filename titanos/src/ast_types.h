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
typedef enum _AstType
{
    AST_STRUCT_MEMBER,
    AST_ATTRIBUTE,
    AST_TYPE_EXPR,
    AST_FUNC_DEFINTION,
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
    AST_CONST_EXPR,
    AST_BINARY_EXPR,
    AST_TERNARY_EXPR,
    AST_UNARY_EXPR,
    AST_POST_EXPR,
    AST_IDENTIFIER_EXPR,
    AST_CALL_EXPR,
    AST_SIZEOF_EXPR,
    AST_CAST_EXPR,
    AST_SUBSCRIPT_EXPR,
    AST_ACCESS_EXPR,
    AST_STRUCT_INIT_VALUES_EXPR,
    AST_DESIGNATED_INITIALIZED_EXPR,
    AST_DEFER_RELASE,
    AST_ASM_STMT,
} AstType;

typedef struct _DeferList
{
    Ast *defer_start;
    Ast *defer_end;
} DeferList;


typedef struct _AstDeclareStmt
{
    struct _Decl *decl;
} AstDeclareStmt;

typedef struct _AstSizeofExpr
{
    Ast *expr;
} AstSizeofExpr;

typedef struct _AstCastExpr
{
    Ast *expr;
    Ast *type;
} AstCastExpr;


typedef struct _AstTernaryExpr
{
    Ast *expr;
    Ast *true_expr; // May be null for elvis!
    Ast *false_expr;
} AstTernaryExpr;



typedef struct _AstIdentifierExpr
{
    Token identifier;
    Ast *resolved;
} AstIdentifierExpr;


typedef struct _AstCallExpr
{
    Ast *function;
    Vector *parameters;
} AstCallExpr;

typedef struct _AstSubscriptExpr
{
    Ast *expr;
    Ast *index;
} AstSubscriptExpr;

typedef struct _AstAccessExpr
{
    Ast *parent;
    Ast *sub_element;
} AstAccessExpr;

typedef double float_type;

typedef struct _AstFloatExpr
{
    float_type f;
} AstFloatExpr;


typedef struct _AstConstExpr
{
    Value value;
} AstConstExpr;


typedef struct _AstCompoundStmt
{
    Vector *stmts; // May be NULL!
    DeferList defer_list;
} AstCompoundStmt;


typedef struct _AstIfStmt
{
    Ast *expr;
    Ast *if_body;
    Ast *else_body;
} AstIfStmt;

typedef struct _AstDefaultStmt
{
    Vector *stmts;
    DeferList defer_list;
} AstDefaultStmt;

typedef struct _AstCaseStmt
{
    Ast *expr;
    Vector *stmts;
    DeferList defer_list;
} AstCaseStmt;

typedef struct _AstForStmt
{
    Ast *init;
    Ast *cond;
    Ast *incr;
    Ast *body;
} AstForStmt;

struct _AstDoWhileStmt
{
    Ast *expr;
    Ast *body;
};

typedef struct _AstDoWhileStmt AstDoStmt;
typedef struct _AstDoWhileStmt AstWhileStmt;

typedef struct _AstSwitchStmt
{
    Ast *expr;
    Vector *case_list;
    Ast *default_stmt;
} AstSwitchStmt;


typedef struct _AstDecl
{
    bool is_public : 1;
    bool is_exported : 1;
    bool is_parameter : 1;
    bool is_used : 1;
    Token name;
    Ast *init_expr; // May be NULL!
    Ast *type;
} AstDecl;

typedef struct _Type Type;

typedef struct _AstFuncDefinition
{
    bool is_public : 1;
    bool is_exported : 1;
    Type *func_decl;
    Ast *body; // AstCompoundStmt will be NULL in interfaces.
    Vector *defers; // NULL unless defers
} AstFuncDefinition;

typedef struct _AstStructInitValuesExpr
{
    Vector *values;
} AstStructInitValuesExpr;

typedef struct _AstGotoStmt
{
    union
    {
        Ast *label;
        Ast *label_stmt;
    };
    DeferList defer_list;

} AstGotoStmt;

typedef struct _AstReturnStmt
{
    Ast *expr; // May be NULL
    Ast *defer_top;
} AstReturnStmt;

typedef struct _AstDeferStmt
{
    bool emit_boolean : 1;
    Ast *body;
    Ast *prev_defer;
} AstDeferStmt;

typedef struct _AstBinaryExpr
{
    struct _Ast* left;
    struct _Ast* right;
    token_type operator;
} AstBinaryExpr;

typedef struct _AstStringExpr
{
    Token string;
    struct _Ast* next_string;
} AstStringExpr;

typedef struct _AstDesignatedInitializerExpr
{
    Token identifer;
    struct _Ast* expr;
} AstDesignatedInitializerExpr;

typedef struct _AstUnaryExpr
{
    struct _Ast* expr;
    token_type operator;
} AstUnaryExpr;

typedef struct _AstPostExpr
{
    struct _Ast* expr;
    token_type operator;
} AstPostExpr;

typedef struct _AstLabel
{
    bool is_used : 1;
    Token label_name;
    Ast *defer_top;
} AstLabelStmt;


typedef struct _TypeExprFlags
{
    bool local : 1;
    bool volatile_ref : 1;
    bool const_ref : 1;
    bool alias_ref : 1;
    bool resolved : 1;
} TypeExprFlags;

typedef struct _QualifierType QualifierType;


typedef struct _IdentifierType
{
    Token module_name;
    Token name;
    Ast *resolved_type;
} IdentifierTypeExpr;

typedef struct _ArrayTypeExpr
{
    Ast *type; // TypeExpr
    Ast *size;
} ArrayTypeExpr;

typedef struct _PointerTypeExpr
{
    Ast *type;
} PointerTypeExpr;

typedef enum _TypeExprType
{
    TYPE_EXPR_IDENTIFIER,
    TYPE_EXPR_ARRAY,
    TYPE_EXPR_POINTER,
    TYPE_EXPR_VOID,
} TypeExprType;

typedef struct _AstAttributeList
{
    Vector *list;
} AstAttributeList;

typedef struct _AstTypeExpr
{
    TypeExprType type;
    TypeExprFlags flags;
    union
    {
        IdentifierTypeExpr identifier_type_expr;
        ArrayTypeExpr array_type_expr;
        PointerTypeExpr pointer_type_expr;
    };
} AstTypeExpr;

typedef struct _AstAttribute
{
    Token name;
    Ast *value;
} AstAttribute;




typedef struct _AstDeferReleaseStmt
{
    Ast *inner;
    DeferList list;
} AstDeferReleaseStmt;

typedef enum
{
    CONST_UNKNOWN,
    CONST_FULL,
    CONST_NONE
} AstConstState;

typedef struct _Ast
{
    AstType type : 8;
    AstConstState const_state : 2;
    Token span;
    union {

        AstAttribute attribute;

        AstTypeExpr type_expr;

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
        AstDecl decl;
        AstLabelStmt label_stmt;
        AstDeferReleaseStmt defer_release_stmt;

        AstConstExpr const_expr;
        AstBinaryExpr binary_expr;
        AstTernaryExpr ternary_expr;
        AstUnaryExpr unary_expr;
        AstIdentifierExpr identifier_expr;
        AstCallExpr call_expr;
        AstSubscriptExpr subscript_expr;
        AstAccessExpr access_expr;
        AstPostExpr post_expr;
        AstStructInitValuesExpr struct_init_values_expr;
        AstDesignatedInitializerExpr designated_initializer_expr;
        AstSizeofExpr sizeof_expr;
        AstCastExpr cast_expr;

    };
} Ast;

void print_ast(Ast *ast, unsigned current_indent);
void print_sub_ast(const char *header, unsigned current_indent, Ast *ast);
Ast *new_ast(AstType type);
Ast *new_ast_with_span(AstType type, Token *span);
Ast *end_ast(Ast *ast, Token *end);

Ast *new_type_expr(TypeExprType type_expr, Token *span);
Ast *ast_compound_stmt_last(Ast *compound_stmt);