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
    CAST_FPBOOL,
    CAST_UIFP,
    CAST_UIUI,
    CAST_UISI,
    CAST_SIFP,
    CAST_SISI,
    CAST_SIUI,
    CAST_INTBOOL,
    CAST_BOOLFP,
    CAST_BOOLINT,
} CastType;


// IF ORDER IS CHANGED, rewrite type_implicit_convert_ordered
typedef enum _TypeIdOld
{
    XTYPE_INVALID,
    XTYPE_UNRESOLVED,
    XTYPE_RESOLVED,
    XTYPE_VOID,
    XTYPE_STRING,
    XTYPE_OPAQUE,
    XTYPE_POINTER,
    XTYPE_ARRAY,
    XTYPE_ALIAS,
    XTYPE_TYPEVAL,
    XTYPE_ENUM,
    XTYPE_FUNC,
    XTYPE_FUNC_TYPE,
    XTYPE_STRUCT,
    XTYPE_UNION,
    XTYPE_FLOAT, // Note that ordering here is important since it doubles as precedence order during conversion.
    XTYPE_INT,
    XTYPE_BOOL,
    XTYPE_NIL,
    XTYPE_CONST_FLOAT,
    XTYPE_CONST_INT,
    // TYPE_OPAQUE, TYPE_MODULE?, TYPE_NONNULL_PTR? VECTOR ETC
} TypeIdOld;


// IF ORDER IS CHANGED, rewrite type_implicit_convert_ordered
typedef enum _TypeId
{
    TYPE_VOID,
    TYPE_OPAQUE,
    TYPE_F64, // Note that ordering here is important since it doubles as precedence order during conversion.
    TYPE_F32,
    TYPE_CONST_FLOAT,
    TYPE_U64,
    TYPE_I64,
    TYPE_U32,
    TYPE_I32,
    TYPE_U16,
    TYPE_I16,
    TYPE_U8,
    TYPE_I8,
    TYPE_BOOL,
    TYPE_CONST_INT,
    TYPE_NIL,
    TYPE_POINTER,
    TYPE_STRING,
    TYPE_ARRAY,
    TYPE_ALIAS,
    TYPE_FUNC_TYPE,
    TYPE_ENUM,
    TYPE_FUNC,
    TYPE_STRUCT,
    TYPE_UNION,
} TypeId;

enum
{
    MAX_INT_TYPE = TYPE_U64,
    MIN_INT_TYPE = TYPE_I8,
    MAX_FLOAT_TYPE = TYPE_F64,
    MIN_FLOAT_TYPE = TYPE_F32,
};
typedef struct _TypeOld TypeOld;
typedef struct _Expr Expr;
typedef struct _Decl Decl;

typedef struct _TypePointer
{
    TypeOld *base;
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
    TypeOld *base;
} TypeOpaque;


typedef struct _TypeArray
{
    TypeOld *base;
    bool is_len_resolved : 1;
    bool is_empty : 1;
    union
    {
        Expr *len_expr;
        uint32_t len;
    };
} TypeArray;


typedef struct _QualifiedType
{
    struct _Type *type;
    TypeQualifier qualifier;
} QualifiedType;


#define NO_LEN 0xFFFFFFFF
typedef struct _Type
{
    TypeId type_id;
    union
    {
        Decl *decl;
        struct
        {
            QualifiedType base;
            uint32_t len;
        };
    };
    LLVMTypeRef llvm_type;
    QualifiedType canonical_type;
} Type;


typedef struct _TypeOld
{
    TypeIdOld type_id : 8;
    bool is_public : 1;
    bool is_exported : 1;
    bool is_incremental : 1;
    bool is_used_public : 1;
    TypeQualifier qualifier : 4;
    LLVMTypeRef llvm_type;
    TypeOld *canonical;
    struct _Module *module;
    const char *lazy_name;
    union
    {
        TypePointer pointer;
        TypeArray array;
        TypeOld *alias;
        TypeOld *opaque;
        TypeUnresolved unresolved;
        TypeOld *type_of_type;
        Decl *decl;
        TypeInt integer;
        uint16_t float_bits;
        TypeOld *resolved;
    };
} TypeOld;

void type_print(QualifiedType type);
Type *type_new(TypeId type_id);
TypeOld *new_unresolved_type(Expr *expr, bool public);
TypeOld *new_type(TypeIdOld type_id, bool public);
void type_print_sub(const char *header, unsigned int current_indent, TypeOld *type);
QualifiedType void_type(void);
QualifiedType type_nil(void);
QualifiedType type_string(void);
QualifiedType type_compint(void);
QualifiedType type_compfloat(void);
QualifiedType type_new_pointer(QualifiedType base);
bool type_is_signed2(TypeOld *type);
uint64_t type_size(TypeOld *type);
bool type_is_float(Type *type);
bool type_is_same(Type *type1, Type *type2);
const char *type_to_string2(TypeOld *type);
const char *type_to_string(QualifiedType qt);
TypeOld *type_unfold_redirects(TypeOld *type);
TypeOld *type_unfold_non_opaque(TypeOld *type);
bool type_may_convert_to_bool(TypeOld *type);
void type_copy(TypeOld **dest, TypeOld *source);
QualifiedType type_remove_qualifier(QualifiedType type);
QualifiedType type_canonical(QualifiedType type);
bool type_is_aritmetic(Type *canonical_type);

static inline bool type_is_int(Type *type)
{
    if (!type) return false;
    if (type->type_id == TYPE_CONST_INT) return true;
    return type->type_id >= MAX_INT_TYPE && type->type_id <= MIN_INT_TYPE;
}

static inline bool type_is_ptr(Type *type)
{
    return type->type_id == TYPE_NIL || type->type_id == TYPE_POINTER;
}

static inline bool type_is_bool(Type *type)
{
    return type->type_id == TYPE_BOOL;
}

static inline uint64_t type_max(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_U64:
            return UINT64_MAX;
        case TYPE_I64:
            return INT64_MAX;
        case TYPE_U32:
            return UINT32_MAX;
        case TYPE_I32:
            return INT32_MAX;
        case TYPE_U16:
            return UINT16_MAX;
        case TYPE_I16:
            return INT16_MAX;
        case TYPE_U8:
            return UINT8_MAX;
        case TYPE_I8:
            return INT8_MAX;
        default:
            FATAL_ERROR("Should not be used for type");
    }
}
static inline uint16_t type_bits(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_F64:
        case TYPE_U64:
        case TYPE_I64:
            return 64;
        case TYPE_F32:
        case TYPE_U32:
        case TYPE_I32:
            return 32;
        case TYPE_U16:
        case TYPE_I16:
            return 16;
        case TYPE_U8:
        case TYPE_I8:
            return 8;
        case TYPE_CONST_INT:
        case TYPE_CONST_FLOAT:
            return 0;
        default:
            return type_size(type) * 8;
    }
}

static inline bool type_is_signed(Type *type)
{
    assert(type_is_int(type));
    if (type->type_id == TYPE_CONST_INT) return true;
    return ((type->type_id - TYPE_I32) % 2) == 0;
}

/**
 * Return types in conversion order.
 * @param first
 * @param second
 * @return true if first should be ordered first, false otherwise
 */
bool type_order(TypeOld *first, TypeOld *second);
static inline bool type_is_const2(TypeOld *type)
{
    return type->qualifier & TYPE_QUALIFIER_CONST;
}

#define IS_CONST(qt) (qt.qualifier & TYPE_QUALIFIER_CONST)
#define IS_VOLATILE(qt) (qt.qualifier & TYPE_QUALIFIER_VOLATILE)
#define IS_ALIAS(qt) (qt.qualifier & TYPE_QUALIFIER_ALIAS)
#define IS_INVALID(qt) (qt.type == NULL)
extern QualifiedType InvalidType;
