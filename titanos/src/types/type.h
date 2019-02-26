#pragma once
//
// Created by Christoffer Lernö on 2019-02-03.
//



#include <llvm-c/Types.h>
#include "common.h"
#include "lexer.h"

typedef enum _TypeQualifier
{
    TYPE_QUALIFIER_NONE = 0,
    TYPE_QUALIFIER_CONST = 0x01,
    TYPE_QUALIFIER_VOLATILE = 0x02,
    TYPE_QUALIFIER_ALIAS = 0x04,
} TypeQualifier;

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
    TYPE_RESOLVED,
    TYPE_VOID,
    TYPE_STRING,
    TYPE_OPAQUE,
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_ALIAS,
    TYPE_TYPEVAL,
    TYPE_ENUM,
    TYPE_FUNC,
    TYPE_FUNC_TYPE,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_FLOAT, // Note that ordering here is important since it doubles as precedence order during conversion.
    TYPE_INT,
    TYPE_BOOL,
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
    Type *base;
} TypePointer;

typedef struct _TypeInt
{
    uint16_t bits;
    bool is_signed;
} TypeInt;

typedef struct _TypeUnresolved
{
    Expr *type_expr;
} TypeUnresolved;

typedef struct _TypeOpaque
{
    Type *base;
} TypeOpaque;


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
    TypeQualifier qualifier : 4;
    LLVMTypeRef llvm_type;
    Type *canonical;
    struct _Module *module;
    const char *lazy_name;
    union
    {
        TypePointer pointer;
        TypeArray array;
        Type *alias;
        Type *opaque;
        TypeUnresolved unresolved;
        Type *type_of_type;
        Decl *decl;
        TypeInt integer;
        uint16_t float_bits;
        Type *resolved;
    };
} Type;

Type *new_unresolved_type(Expr *expr, bool public);
Type *new_type(TypeId type_id, bool public);
void type_print_sub(const char *header, unsigned int current_indent, Type *type);
Type *void_type(void);
Type *type_nil(void);
Type *type_string(void);
Type *type_invalid(void);
Type *type_compint(void);
Type *type_compfloat(void);
Type *type_new_pointer(Type *base);
Type *type_new_array(Type *base);
Type *type_to_type(Type *type);
bool type_is_int(Type *type);
bool type_is_signed(Type *type);
uint64_t type_size(Type *type);
bool type_is_same(Type *type1, Type *type2);
const char *type_to_string(Type *type);
Type *type_unfold_redirects(Type *type);
Type *type_unfold_non_opaque(Type *type);
bool type_may_convert_to_bool(Type *type);
void type_copy(Type **dest, Type *source);

/**
 * Return types in conversion order.
 * @param first
 * @param second
 * @return true if first should be ordered first, false otherwise
 */
bool type_order(Type *first, Type *second);
static inline bool type_is_const(Type *type)
{
    return type->qualifier & TYPE_QUALIFIER_CONST;
}