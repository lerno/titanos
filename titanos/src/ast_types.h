#pragma once
//
// Created by Christoffer Lerno on 2018-12-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "lexer.h"
#include "common.h"
#include "vector.h"
#include "component.h"

typedef enum _AstType
{
    AST_STRUCT_MEMBER,
    AST_ATTRIBUTE_LIST,
    AST_ATTRIBUTE,
    AST_TYPE_EXPR,
    AST_FUNC_DECL,
    AST_FUNC_DEFINTION,
    AST_PARAM_LIST,
    AST_PARAM_DECL,
    AST_COMPOUND_STMT,
    AST_IF_STMT,
    AST_WHILE_STMT,
    AST_DO_STMT,
    AST_DEFER_STMT,
    AST_SWITCH_STMT,
    AST_CASE_STMT,
    AST_DEFAULT_STMT,
    AST_BREAK_STMT,
    AST_CONTINUE_STMT,
    AST_RETURN_STMT,
    AST_GOTO_STMT,
    AST_FOR_STMT,
    AST_DECLARATION,
    AST_LABEL,
    AST_FLOAT_EXPR,
    AST_BOOL_EXPR,
    AST_INT_EXPR,
    AST_NIL_EXPR,
    AST_STRING_EXPR,
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
    AST_VAR_DEFINITION,
    AST_BUILTIN_TYPE,
    AST_STRUCT_TYPE,
    AST_ALIAS_TYPE,
    AST_ENUM_TYPE,
    AST_FUNC_TYPE,
    AST_ENUM_ENTRY,
    AST_INCREMENTAL_ARRAY,
} AstType;

typedef enum _BuiltinFamily
{
    BUILTIN_FLOAT,
    BUILTIN_BOOL,
    BUILTIN_UINT,
    BUILTIN_INT,
} BuiltinFamily;

typedef struct _AstBuiltinType
{
    BuiltinFamily type : 3;
    unsigned bits : 13;
} AstBuiltinType;

typedef struct _AstSizeofExpr
{
    struct _Ast *expr;
} AstSizeofExpr;

typedef struct _AstCastExpr
{
    struct _Ast *expr;
    struct _Ast *type;
} AstCastExpr;

typedef struct _AstVarDefinition
{
    bool is_public : 1;
    Token name;
    struct _Ast *type;
    struct _Ast *attributes;
    struct _Ast *value; // May be NULL
} AstVarDefinition;

typedef struct _AstIncrementalArray
{
    Token name;
    struct _Ast *value;
} AstIncrementalArray;

typedef struct _AstTernaryExpr
{
    struct _Ast *expr;
    struct _Ast *true_expr; // May be null for elvis!
    struct _Ast *false_expr;
} AstTernaryExpr;

typedef struct _AstEnumEntry
{
    Token name;
    struct _Ast *value; // MAY BE NULL
} AstEnumEntry;

typedef struct _AstIdentifierExpr
{
    Token identifier;
} AstIdentifierExpr;


typedef struct _AstCallExpr
{
    struct _Ast *function;
    Vector *parameters;
} AstCallExpr;

typedef struct _AstSubscriptExpr
{
    struct _Ast *expr;
    struct _Ast *index;
} AstSubscriptExpr;

typedef struct _AstAccessExpr
{
    struct _Ast *parent;
    struct _Ast *sub_element;
} AstAccessExpr;

typedef struct _AstFloatExpr
{
    double f;
} AstFloatExpr;

typedef struct _AstAliasType
{
    bool is_public : 1;
    Token alias;
    struct _Ast *type_definition;
} AstAliasType;

typedef struct _AstEnumType
{
    bool is_public : 1;
    bool is_incremental : 1;
    Token name;
    struct _Ast *type; // Will be an identifier!
    Vector *entries; // AstEnumEntry
    struct _Ast *attributes; // May be NULL
} AstEnumType;

typedef struct _AstFuncType
{
    bool is_public : 1;
    Token name;
    struct _Ast *declaration; // AstFuncDeclaration
} AstFuncType;

typedef struct _AstIntExpr
{
    union
    {
        int64_t i;
        uint64_t u;
    };
    char sign;
} AstIntExpr;

typedef struct _AstBoolExpr
{
    bool i;
} AstBoolExpr;

typedef struct _AstParamList
{
    bool variadic : 1;
    Vector *param_list;
} AstParamList;

typedef struct _AstCompoundStmt
{
    Vector *stmts; // May be NULL!
} AstCompoundStmt;

typedef struct _AstDeclaration
{
    Token identifier;
    struct _Ast *initExpr; // May be NULL!
    struct _Ast *declType;
} AstDeclaration;

typedef struct _AstIfStmt
{
    struct _Ast *expr;
    struct _Ast *if_body;
    struct _Ast *else_body;
} AstIfStmt;

typedef struct _AstDefaultStmt
{
    Vector *stmts;
} AstDefaultStmt;

typedef struct _AstCaseStmt
{
    struct _Ast *expr;
    Vector *stmts;
} AstCaseStmt;

typedef struct _AstForStmt
{
    struct _Ast *init;
    struct _Ast *cond;
    struct _Ast *incr;
    struct _Ast *body;
} AstForStmt;

struct _AstDoWhileStmt
{
    struct _Ast *expr;
    struct _Ast *body;
};

typedef struct _AstDoWhileStmt AstDoStmt;
typedef struct _AstDoWhileStmt AstWhileStmt;

typedef struct _AstSwitchStmt
{
    struct _Ast *expr;
    Vector *case_list;
    struct _Ast *default_stmt;
} AstSwitchStmt;

typedef struct _AstFuncDecl
{
    struct _Ast *r_type;
    struct _Ast *params;
    struct _Ast *name; // AstTypeExpr
    struct _Ast *attributes;
} AstFuncDecl;

typedef struct _AstParamDecl
{
    struct _Ast *type;
    Token name; //
    struct _Ast *defaultValue; // May be NULL!
} AstParamDecl;

typedef struct _AstFuncDefinition
{
    bool is_public : 1;
    struct _Ast *func_decl; // AstFuncDecl
    struct _Ast *body; // AstCompoundStmt
} AstFuncDefinition;

typedef struct _AstStructInitValuesExpr
{
    Vector *values;
} AstStructInitValuesExpr;

typedef struct _AstGotoStmt
{
    struct _Ast *label;
} AstGotoStmt;

typedef struct _AstReturnStmt
{
    struct _Ast *expr; // May be NULL
} AstReturnStmt;

typedef struct _AstDeferStmt
{
    struct _Ast* body;
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
    Token label_name;
} AstLabelStmt;


typedef struct _TypeExprFlags
{
    bool local : 1;
    bool volatile_ref : 1;
    bool const_ref : 1;
    bool alias_ref : 1;
    bool in_evaluation : 1;
    bool resolved : 1;
} TypeExprFlags;

typedef struct _QualifierType QualifierType;


typedef struct _UnresolvedIdentifier
{
    Token module_name;
    Token name;
} UnresolvedIdentifier;

typedef struct _ArrayTypeExpr
{
    struct _Ast *type; // Note that this may either be a type or expression
    struct _Ast *size;
} ArrayTypeExpr;

typedef struct _PointerTypeExpr
{
    struct _Ast *type;
} PointerTypeExpr;

typedef enum _TypeExprType
{
    TYPE_EXPR_UNRESOLVED_IDENTIFIER,
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
        UnresolvedIdentifier unresolved_identifier;
        ArrayTypeExpr array_type_expr;
        PointerTypeExpr pointer_type_expr;
    };
} AstTypeExpr;

typedef struct _AstAttribute
{
    Token name;
    struct _Ast *value;
} AstAttribute;

typedef struct _AstStructType
{
    bool is_struct : 1;
    bool is_public : 1;
    Token name;
    Vector *members;
    struct _Ast *attribute_list; // may be NULL
} AstStructType;

typedef enum _StructMemberType
{
    STRUCT_MEMBER_TYPE_NORMAL,
    STRUCT_MEMBER_TYPE_STRUCT,
    STRUCT_MEMBER_TYPE_UNION
} StructMemberType;

typedef struct _AstStructMember
{
    StructMemberType type : 2;
    Token name;
    union
    {
        Vector *members;
        struct _Ast *value_type;
    };
} AstStructMember;



typedef struct _Ast
{
    AstType type;
    Token span;
    union {

        AstFuncDecl func_decl;
        AstFuncDefinition func_definition;
        AstParamList param_list;
        AstAttributeList attribute_list;
        AstAttribute attribute;
        AstParamDecl param_decl;

        AstStructType struct_type;
        AstTypeExpr type_expr;
        AstStructMember struct_member;
        AstAliasType alias_type;
        AstEnumType enum_type;
        AstEnumEntry enum_entry;
        AstFuncType func_type;
        AstIncrementalArray incremental_array;
        AstVarDefinition var_definition;
        AstBuiltinType builtin_type;

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
        AstDeclaration declaration;
        AstLabelStmt label_stmt;

        AstFloatExpr float_expr;
        AstIntExpr int_expr;
        AstBoolExpr bool_expr;
        AstStringExpr string_expr;
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

void print_ast(Ast *ast, int current_indent);

Ast *new_ast(AstType type);
Ast *new_ast_with_span(AstType type, Token *span);
Ast *end_ast(Ast *ast, Token *end);
