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
    DECL_UNION_TYPE,
    DECL_ENUM_TYPE,
    DECL_FUNC_TYPE,
    DECL_ARRAY_VALUE,
    DECL_IMPORT,
    DECL_LABEL,
    DECL_MACRO,
    DECL_MACRO_PARAM
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
    bool local : 1;
    union
    {
        struct
        {
            QualifiedType original_type;
            Expr *init_expr;
            LLVMValueRef llvm_ref;
        };
        struct
        {
            SourceRange type_range;
            SourceRange init_range;
        } unparsed;

    };

} VarDecl;

typedef struct _MacroParamDecl
{
    bool is_ref : 1;
    Expr *init_expr;
} MacroParmDecl;

typedef struct _ArrayValueDecl
{
    TypeOld *value;
    LLVMValueRef ir_value;
} ArrayValueDecl;

typedef struct _EnumConstantDecl
{
    Expr *init_value;
    Value value;
    LLVMValueRef llvm_value;
} EnumConstantDecl;

typedef struct _EnumDecl
{
    bool incremental : 1;
    union
    {
        struct
        {
            struct _Vector *constants;
            QualifiedType type;
            LLVMValueRef llvm_value;
        };
        struct
        {
            SourceRange body;
            SourceRange type;
        } unparsed;

    };
} EnumDecl;

typedef enum _StructType
{
    ST_STRUCT,
    ST_UNION
} StructType;

typedef struct _StructDecl
{
    bool is_global : 1;
    bool no_typedef : 1;
    union
    {
        struct
        {
            Vector *members;;
        };
        struct
        {
            SourceRange body;
        } unparsed;
    };
    Vector *struct_functions;
} StructDecl;


typedef struct _FuncDecl
{
    bool variadic : 1;
    bool is_struct_func : 1;
    bool is_static_struct_func : 1;
    const char *full_name;
    union
    {
        struct
        {
            QualifiedType rtype;
            Vector* args; // VarDecl[]
            Ast *body;
            LLVMValueRef llvm_function_proto;
        };
        struct
        {
            SourceRange rtype;
            SourceRange params;
            SourceRange body;
        } unparsed;
    };

} FuncDecl;

typedef struct _MacroDecl
{
    bool variadic : 1;
    const char *full_name;
    Vector* args; // VarDecl[]
    Ast *body;
} MacroDecl;

typedef struct _FunTypeDecl
{
    struct _Decl *func_decl;

} FuncTypeDecl;

typedef enum
{
    IMPORT_TYPE_FULL,
    IMPORT_TYPE_ALIAS,
    IMPORT_TYPE_LOCAL,
} ImportType;

typedef struct _ImportDecl
{
    ImportType type : 2;
    const char *alias;
} ImportDecl;


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
    bool is_unparsed : 1;
    bool is_being_parsed : 1;
    SourceRange span;
    const char *name;
    SourceRange name_span;
    SourceLoc loc;
    struct _Module *module;
    struct _Vector *attributes;
    union
    {
        QualifiedType self_type;
        QualifiedType type;
    };
    union
    {
        QualifiedType alias;
        SourceRange  unparsed_alias;
        StructDecl struct_decl;
        VarDecl var;
        EnumDecl enum_decl;
        EnumConstantDecl enum_constant;
        FuncDecl func_decl;
        MacroDecl macro_decl;
        FuncTypeDecl func_type;
        ImportDecl import;
        ArrayDecl array_decl;
        MacroParmDecl macro_param;
    };
} Decl;

void decl_init(Decl *decl, DeclType decl_type, SourceRange span, const char *name, bool public);
Decl *decl_new(DeclType decl_type, SourceRange span, const char *name, bool public);
Decl *decl_new2(DeclType decl_type, SourceLoc start, Token *name, bool public);
void decl_add_own_type(Decl *decl, TypeId type_id);

void decl_print(Decl *decl, unsigned indent);
void decl_print_sub(const char *header, unsigned current_indent, Decl *decl);
void decl_print_attributes(Decl *decl, unsigned indent);
bool decl_has_attribute(Decl *decl, enum _AttributeType attribute);
bool decl_is_type(Decl *decl);
uint64_t decl_size(Decl *decl);