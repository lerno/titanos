#pragma once
//
// Created by Christoffer Lernö on 2019-02-03.
//



#include <llvm-c/Types.h>
#include "common.h"
#include "lexer.h"

typedef enum _TypeId
{
    TYPE_INVALID,
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_CONST_FLOAT,
    TYPE_CONST_INT,
    TYPE_NIL,
    TYPE_ENUM,
    TYPE_FUNC,
    TYPE_UNION,
    TYPE_OPAQUE,
    TYPE_IMPORT,
    TYPE_UNRESOLVED,
    // TYPE_OPAQUE, TYPE_MODULE?, TYPE_NONNULL_PTR? VECTOR ETC
} TypeId;


typedef struct _Type Type;
typedef struct _Ast Ast;

typedef struct _TypePointer
{
    bool is_const : 1;
    bool is_volatile : 1;
    Type *base;
} TypePointer;

typedef struct _TypeInt
{
    uint32_t bits;
    bool is_signed : 1;
} TypeInt;

typedef struct _TypeUnresolved
{
    Ast *type_expr;
    Token module_name;
    Token identifier;
} TypeUnresolved;

typedef struct _TypeOpaque
{
    Type *base;
} TypeOpaque;
typedef struct _TypeFloat
{
    uint32_t bits;
} TypeFloat;

typedef struct _TypeArray
{
    Type *base;
    uint32_t len;
    bool is_incremental;
} TypeArray;

typedef struct _TypeStructUnion
{
    struct _Ast *decl;
} TypeStructUnion;

typedef struct _FunctionName
{
    Token struct_name;
    Token function_name;
    Token full_name;
} FunctionName;

typedef struct _TypeFunc
{
    bool rtype_resolved : 1;
    FunctionName *name;
    union
    {
        Ast *rtype_expr;
        Type *rtype;
    };
    Ast *params;
} TypeFunc;

typedef struct _TypeEnum
{
    struct _Ast *decl;
    struct _Vector *entries;
    Type *int_type;
    /*
    // set this flag temporarily to detect infinite loops
    bool embedded_in_current;
    bool reported_infinite_err;
    // whether we've finished resolving it
    bool complete;*/
} TypeEnum;

typedef struct _Type
{

    TypeId type_id : 5;
    bool is_public : 1;
    bool is_exported : 1;
    bool is_incremental : 1;
    bool is_used_public : 1;

    Token span;
    Token name;
    Token module_name;
    LLVMTypeRef llvm_type;
    struct _Module *module;
    struct _Ast *attributes;

    union
    {
        TypePointer pointer;
        TypeInt integer;
        TypeFloat real;
        TypeArray array;
        TypeStructUnion structure;
        TypeStructUnion unionstruct;
        TypeEnum enumeration;
        TypeFunc func;
        TypeOpaque opaque;
        TypeUnresolved unresolved;
    };
} Type;

Type *new_unresolved_type(Ast *expr, bool public);
Type *new_type(TypeId type_id, bool public, Token *initial_token);
Type *end_type(Type *ast, Token *end);
void print_type(Type *type, int current_indent);
void print_sub_type(const char *header, int current_indent, Type *type);
