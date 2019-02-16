#pragma once
//
// Created by Christoffer Lernö on 2019-02-08.
//

#include "lexer.h"
#include "common.h"
#include "vector.h"
#include "component.h"
#include "value.h"
#include "types/type.h"

typedef struct _Ast Ast;
typedef struct _Type Type;
typedef struct _Expr Expr;
typedef struct _Decl Decl;

typedef enum _ExprTypeId
{
    EXPR_TYPE,
    EXPR_CONST,
    EXPR_BINARY,
    EXPR_TERNARY,
    EXPR_UNARY,
    EXPR_POST,
    EXPR_IDENTIFIER,
    EXPR_CALL,
    EXPR_SIZEOF,
    EXPR_CAST,
    EXPR_SUBSCRIPT,
    EXPR_ACCESS,
    EXPR_STRUCT_INIT_VALUES,
    EXPR_DESIGNATED_INITIALIZER,
} ExprTypeId;

typedef struct _ExprSizeof
{
    Expr *expr;
} ExprSizeof;

typedef struct _ExprCast
{
    Expr *expr;
    Type *type;
    bool implicit;
    CastResult cast_result;
} ExprCast;


typedef struct _ExprTernary
{
    Expr *cond;
    Expr *then_expr; // May be null for elvis!
    Expr *else_expr;
} ExprTernary;



typedef struct _ExprIdentifier
{
    Token identifier;
    Decl *resolved;
} ExprIdentifier;


typedef struct _ExprCall
{
    Expr *function;
    Vector *parameters;
} ExprCall;

typedef struct _ExprSubscript
{
    Expr *expr;
    Expr *index;
} ExprSubscript;

typedef struct _ExprAccess
{
    Expr *parent;
    Expr *sub_element;
} ExprAccess;


typedef struct _ExprConst
{
    Value value;
} ExprConst;

typedef struct _ExprStructInitValues
{
    Vector *values;
} ExprStructInitValues;


typedef struct _ExprBinary
{
    Expr *left;
    Expr *right;
    token_type operator;
} ExprBinary;

typedef struct _ExprDesignatedInitializer
{
    Token identifer;
    Expr *expr;
} ExprDesignatedInitializer;

typedef struct _ExprUnary
{
    Expr* expr;
    token_type operator;
} ExprUnary;

typedef struct _ExprPost
{
    Expr *expr;
    token_type operator;
} ExprPost;


typedef struct _TypeExprFlags
{
    bool local : 1;
    bool volatile_ref : 1;
    bool const_ref : 1;
    bool alias_ref : 1;
    bool resolved : 1;
} TypeExprFlags;



typedef struct _ExprType
{
    TypeExprFlags flags;
    Type *type;
} ExprType;


typedef enum
{
    CONST_UNKNOWN,
    CONST_FULL,
    CONST_NONE
} ExprConstState;

typedef struct _Expr
{
    ExprTypeId expr_id : 8;
    ExprConstState const_state : 2;
    bool is_evaluating : 1;
    Token span;
    Type *type;
    union {
        ExprType type_expr;
        ExprConst const_expr;
        ExprBinary binary_expr;
        ExprTernary ternary_expr;
        ExprUnary unary_expr;
        ExprIdentifier identifier_expr;
        ExprCall call_expr;
        ExprSubscript subscript_expr;
        ExprAccess access_expr;
        ExprPost post_expr;
        ExprStructInitValues struct_init_values_expr;
        ExprDesignatedInitializer designated_initializer_expr;
        ExprSizeof sizeof_expr;
        ExprCast cast_expr;

    };
} Expr;

void expr_print(Expr *expr, unsigned current_indent);
void expr_print_sub(const char *header, unsigned current_indent, Expr *expr);
Expr *expr_new(ExprTypeId type, Token *span);
Expr *expr_copy(Expr *expr);
Expr *expr_new_type_expr(Type *type, Token *span);
void expr_replace(Expr *target, Expr *source);