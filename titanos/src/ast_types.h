#pragma once
//
// Created by Christoffer Lerno on 2018-12-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "lexer.h"
#include "common.h"
#include "vector.h"
#include "component.h"

typedef struct _Ast Ast;
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
    AST_UINT_EXPR,
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
    AST_TYPE_DEFINITION,
    AST_ENUM_ENTRY,
    AST_INCREMENTAL_ARRAY,
    AST_IMPORT
} AstType;


typedef struct _AstSizeofExpr
{
    Ast *expr;
} AstSizeofExpr;

typedef struct _AstCastExpr
{
    Ast *expr;
    Ast *type;
} AstCastExpr;

typedef struct _AstVarDefinition
{
    bool is_public : 1;
    bool is_exported : 1;
    Token name;
    Ast *type;
    Ast *attributes;
    Ast *value; // May be NULL
} AstVarDefinition;

typedef struct _AstIncrementalArray
{
    Token name;
    Ast *value;
} AstIncrementalArray;

typedef struct _AstTernaryExpr
{
    Ast *expr;
    Ast *true_expr; // May be null for elvis!
    Ast *false_expr;
} AstTernaryExpr;

typedef struct _AstEnumEntry
{
    Token name;
    Ast *value; // MAY BE NULL
} AstEnumEntry;

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

typedef struct _AstFloatExpr
{
    double f;
} AstFloatExpr;

typedef enum _DefinitionType
{
    BUILTIN_TYPE,
    STRUCT_TYPE,
    ALIAS_TYPE,
    ENUM_TYPE,
    FUNC_TYPE,
} DefinitionType;

typedef enum _BuiltinFamily
{
    BUILTIN_FLOAT,
    BUILTIN_BOOL,
    BUILTIN_UINT,
    BUILTIN_INT,
} BuiltinFamily;

typedef struct _DefBuiltin
{
    BuiltinFamily type : 3;
    unsigned bits : 13;
} DefBuiltin;

typedef struct _DefAlias
{
    Ast *type_definition;
} DefAlias;

typedef struct _DefEnum
{
    Ast *type; // Will be an identifier!
    Vector *entries; // AstEnumEntry
} DefEnum;


typedef struct _DefFunc
{
    Ast *declaration; // AstFuncDeclaration
} DefFunc;


typedef struct _DefStruct
{
    Vector *members;
} DefStruct;

typedef enum _StructMemberType
{
    STRUCT_MEMBER_TYPE_NORMAL,
    STRUCT_MEMBER_TYPE_STRUCT,
    STRUCT_MEMBER_TYPE_UNION
} StructMemberType;

typedef struct _AstDefinition
{
    DefinitionType definition_type : 3;
    bool is_public : 1;
    bool is_exported : 1;
    bool is_incremental : 1;
    bool is_struct : 1;
    bool is_used_public : 1;
    Token name;
    Module *module;
    Ast *attributes; // May be NULL
    union
    {
        DefAlias def_alias;
        DefFunc def_func;
        DefEnum def_enum;
        DefBuiltin def_builtin;
        DefStruct def_struct;
    };
} AstDefinition;


typedef struct _AstIntExpr
{
    int64_t i;
} AstIntExpr;

typedef struct _AstUIntExpr
{
    uint64_t u;
} AstUIntExpr;

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
    Ast *initExpr; // May be NULL!
    Ast *declType;
} AstDeclaration;

typedef struct _AstIfStmt
{
    Ast *expr;
    Ast *if_body;
    Ast *else_body;
} AstIfStmt;

typedef struct _AstDefaultStmt
{
    Vector *stmts;
} AstDefaultStmt;

typedef struct _AstCaseStmt
{
    Ast *expr;
    Vector *stmts;
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

typedef struct _FunctionName
{
    Token struct_name;
    Token function_name;
    Token full_name;
} FunctionName;

typedef struct _AstFuncDecl
{
    Ast *r_type;
    Ast *params;
    FunctionName *name; // AstTypeExpr
    Ast *attributes;
} AstFuncDecl;

typedef struct _AstParamDecl
{
    Ast *type;
    Token name; //
    Ast *defaultValue; // May be NULL!
} AstParamDecl;

typedef struct _AstFuncDefinition
{
    bool is_public : 1;
    bool is_exported : 1;
    Ast *func_decl; // AstFuncDecl
    Ast *body; // AstCompoundStmt will be NULL in interfaces.
} AstFuncDefinition;

typedef struct _AstStructInitValuesExpr
{
    Vector *values;
} AstStructInitValuesExpr;

typedef struct _AstGotoStmt
{
    Ast *label;
} AstGotoStmt;

typedef struct _AstReturnStmt
{
    Ast *expr; // May be NULL
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
    Ast *type; // Note that this may either be a type or expression
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


typedef struct _AstStructMember
{
    StructMemberType type : 2;
    Token name;
    union
    {
        Vector *members;
        Ast *value_type;
    };
} AstStructMember;


typedef enum
{
    IMPORT_TYPE_FULL,
    IMPORT_TYPE_ALIAS,
    IMPORT_TYPE_LOCAL,
} ImportType;

typedef struct _AstImport
{
    ImportType type : 3;
    bool used : 1;
    bool used_public : 1;
    Token module_name;
    Token alias;
    Module *module;
} AstImport;

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

        AstFuncDecl func_decl;
        AstFuncDefinition func_definition;
        AstParamList param_list;
        AstAttributeList attribute_list;
        AstAttribute attribute;
        AstParamDecl param_decl;

        AstTypeExpr type_expr;
        AstStructMember struct_member;
        AstDefinition definition;
        AstEnumEntry enum_entry;
        AstIncrementalArray incremental_array;
        AstVarDefinition var_definition;

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
        AstUIntExpr uint_expr;
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

        AstImport import;
    };
} Ast;

void print_ast(Ast *ast, int current_indent);

Ast *new_ast(AstType type);
Ast *new_ast_with_span(AstType type, Token *span);
Ast *end_ast(Ast *ast, Token *end);
Ast *new_type_expr(TypeExprType type_expr, Token *span);
Ast *new_type_definition(DefinitionType type, Token *name, bool public, Token *initial_token);
