#pragma once
//
// Created by Christoffer Lernö on 2019-02-03.
//



#include <llvm-c/Types.h>
#include "common.h"
#include "lexer.h"

typedef enum _CastResult
{
    CAST_INLINE,
    CAST_FAILED,
    CAST_PTRPTR,
    CAST_INTPTR,
    CAST_PTRINT,
    CAST_FPFP,
    CAST_FPUI,
    CAST_FPSI,
    CAST_UIFP,
    CAST_UIUI,
    CAST_UISI,
    CAST_SIFP,
    CAST_SISI,
    CAST_SIUI
} CastResult;


// IF ORDER IS CHANGED, rewrite type_implicit_convert_ordered
typedef enum _TypeId
{
    TYPE_INVALID,
    TYPE_UNRESOLVED,
    TYPE_IMPORT,
    TYPE_VOID,
    TYPE_STRING,
    TYPE_OPAQUE,
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_DECLARED,
    TYPE_TYPEVAL,
    TYPE_BUILTIN,
    TYPE_NIL,
    TYPE_CONST_FLOAT,
    TYPE_CONST_INT,
    // TYPE_OPAQUE, TYPE_MODULE?, TYPE_NONNULL_PTR? VECTOR ETC
} TypeId;


typedef struct _Type Type;
typedef struct _Expr Expr;
typedef struct _Decl Decl;

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
    Expr *type_expr;
} TypeUnresolved;

typedef struct _TypeOpaque
{
    Type *base;
} TypeOpaque;

typedef enum _BuiltinKind {
    BUILTIN_FLOAT,
    BUILTIN_UNSIGNED_INT,
    BUILTIN_SIGNED_INT,
    BUILTIN_BOOL,
} BuiltinKind;


extern const CastResult builtin_casts[4][4];

typedef struct _TypeBuiltin
{
    BuiltinKind builtin_id : 16;
    uint16_t bits;
} TypeBuiltin;

typedef struct _TypeArray
{
    Type *base;
    bool is_len_resolved : 1;
    bool is_empty : 1;
    union
    {
        Expr *len_expr;
        uint32_t len;
    };
} TypeArray;

typedef struct _Type
{
    TypeId type_id : 8;
    bool is_public : 1;
    bool is_exported : 1;
    bool is_incremental : 1;
    bool is_used_public : 1;
    bool is_const : 1;
    bool is_volatile : 1;
    bool is_aliased : 1;
    LLVMTypeRef llvm_type;
    struct _Module *module;
    union
    {
        TypePointer pointer;
        TypeArray array;
        TypeOpaque opaque;
        TypeUnresolved unresolved;
        Type *type_of_type;
        Decl *decl;
        TypeBuiltin builtin;
    };
} Type;

Type *new_unresolved_type(Expr *expr, bool public);
Type *new_type(TypeId type_id, bool public);

void type_print_sub(const char *header, unsigned int current_indent, Type *type);
Type *void_type();
Type *type_nil();
Type *type_string();
Type *type_invalid();
Type *type_compint();
Type *type_compfloat();
Type *type_to_type(Type *type);
bool type_is_int(Type *type);
bool type_is_signed(Type *type);
uint64_t type_size(Type *type);
bool type_is_same(Type *type1, Type *type2);
char *type_to_string(Type *type);
Type *type_unfold_opaque(Type *type);
