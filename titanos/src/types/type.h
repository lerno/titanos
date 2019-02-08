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
typedef struct _TypeFloat
{
    uint32_t bits;
} TypeFloat;

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
    Token span;
    Token name;
    LLVMTypeRef llvm_type;
    Decl *decl;
    struct _Module *module;
    union
    {
        TypePointer pointer;
        TypeInt integer;
        TypeFloat real;
        TypeArray array;
        TypeOpaque opaque;
        TypeUnresolved unresolved;
    };
} Type;

Type *new_unresolved_type(Expr *expr, bool public);
Type *new_type(TypeId type_id, bool public, Token *initial_token);
Type *end_type(Type *ast, Token *end);
void print_type(Type *type, unsigned int current_indent);
void type_print_sub(const char *header, unsigned int current_indent, Type *type);
Type *void_type();