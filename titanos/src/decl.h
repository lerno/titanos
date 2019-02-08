#pragma once
//
// Decl is based on the Decl from C2C
//

#include <types/type.h>
#include <values/value.h>
#include "lexer.h"
#include "vector.h"
#include "attributes.h"

typedef struct _Ast Ast;

typedef enum _DeclType
{
    DECL_BUILTIN = 0,
    DECL_FUNC,
    DECL_VAR,
    DECL_ENUM_CONSTANT,
    DECL_ALIAS_TYPE,
    DECL_STRUCT_TYPE,
    DECL_ENUM_TYPE,
    DECL_FUNC_TYPE,
    DECL_ARRAY_VALUE,
    DECL_IMPORT,
    DECL_LABEL
} DeclType;

typedef enum _VarDeclKind {
    VARDECL_GLOBAL = 0,
    VARDECL_LOCAL,
    VARDECL_PARAM,
    VARDECL_MEMBER
} VarDeclKind;

typedef struct _VarDecl
{
    VarDeclKind kind : 2;
    unsigned local : 1;
    Type *type;
    Expr *init_expr;
    LLVMValueRef ir_value;
} VarDecl;

typedef struct _ArrayValueDecl
{
    Type *value;
    LLVMValueRef ir_value;
} ArrayValueDecl;

typedef struct _EnumConstantDecl
{
    Expr *init_value;
    LLVMValueRef llvm_value;
} EnumConstantDecl;

typedef struct _EnumDecl
{
    bool incremental : 1;
    struct _Vector *constants;
    LLVMValueRef llvm_value;
    Type *type;
} EnumDecl;

typedef enum _StructType
{
    ST_STRUCT,
    ST_UNION
} StructType;

typedef struct _StructDecl
{
    StructType struct_type : 1;
    bool is_global : 1;
    bool no_typedef : 1;
    Vector *members;;
    Vector *struct_functions;
} StructDecl;


typedef struct _FuncDecl
{
    bool variadic : 1;
    bool is_struct_func : 1;
    bool is_static_struct_func : 1;
    Type *rtype;
    //Type *original_r_type;
    Token full_name;
    Vector* args; // VarDecl[]
    Ast *body;
    LLVMTypeRef *ir_proto;
} FuncDecl;

typedef struct _FunTypeDecl
{
    struct _Decl *func_decl;
} FuncTypeDecl;

typedef struct _BuiltinTypeDecl
{
    Type *type;
} BuiltinTypeDecl;

typedef enum
{
    IMPORT_TYPE_FULL,
    IMPORT_TYPE_ALIAS,
    IMPORT_TYPE_LOCAL,
} ImportType;

typedef struct _ImportDecl
{
    ImportType type : 2;
    Token alias;
} ImportDecl;


typedef struct _AliasDecl
{
    Type *type;
} AliasDecl;
typedef struct _LabelDecl
{
    Ast *label_stmt;
    Ast *defer;
} LabelDecl;

typedef struct _ArrayDecl
{
    Expr *value;
} ArrayDecl;

typedef struct _Decl
{
    DeclType type_id : 6;
    bool is_exported : 1;
    bool is_public : 1;
    bool is_used : 1;
    bool is_used_public : 1;
    bool has_cname : 1;
    Token span;
    Token name;
    struct _Module *module;
    struct _Vector *attributes;
    union
    {
        StructDecl struct_decl;
        VarDecl var;
        EnumDecl enum_decl;
        EnumConstantDecl enum_constant;
        FuncDecl func_decl;
        FuncTypeDecl func_type;
        ImportDecl import;
        ArrayDecl array_decl;
        AliasDecl alias_decl;
        BuiltinTypeDecl builtin_decl;
    };
} Decl;

void decl_init(Decl *decl, DeclType decl_type, Token *span, Token *name, bool public);
Decl *decl_new(DeclType decl_type, Token *span, Token *name, bool public);
void decl_print(Decl *decl, unsigned indent);
void decl_print_sub(const char *header, unsigned current_indent, Decl *decl);
void decl_print_attributes(Decl *decl, unsigned indent);
bool decl_has_attribute(Decl *decl, enum _AttributeType attribute);
bool decl_is_type(Decl *decl);