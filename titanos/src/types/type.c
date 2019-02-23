//
// Created by Christoffer Lernö on 2019-02-03.
//

#include <string.h>
#include "printer.h"
#include "expr.h"
#include "type.h"
#include "arena_allocator.h"
#include "ast_types.h"
#include "decl.h"
#include <llvm-c/Core.h>
#include "diagnostics.h"


Type *new_unresolved_type(Expr *expr, bool public)
{
    assert(expr->expr_id == EXPR_TYPE || expr->expr_id == EXPR_IDENTIFIER || expr->expr_id == EXPR_ACCESS);
    Type *type = new_type(TYPE_UNRESOLVED, public);
    type->unresolved.type_expr = expr;
    return type;
}

Type *new_type(TypeId type_id, bool public)
{
    Type *type = malloc_arena(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->type_id = type_id;
    type->is_public = public;

    return type;
}

Type *type_to_type(Type *type)
{
    Type *result = new_type(TYPE_TYPEVAL, type->is_public);
    result->type_of_type = type;
    return result;
}

Type *void_type()
{
    static Type type = { .type_id = TYPE_VOID, .is_public = true };
    return &type;
}

Type *type_nil()
{
    static Type type = { .type_id = TYPE_NIL, .is_public = true };
    return &type;
}

Type *type_string()
{
    static Type type = { .type_id = TYPE_STRING, .is_public = true };
    return &type;
}

Type *type_invalid()
{
    static Type type = { .type_id = TYPE_INVALID, .is_public = true };
    return &type;
}

Type *type_compint()
{
    static Type type = { .type_id = TYPE_CONST_INT, .is_public = true };
    return &type;
}
Type *type_compfloat()
{
    static Type type = { .type_id = TYPE_CONST_FLOAT, .is_public = true };
    return &type;
}

bool type_is_int(Type *type)
{
    return type->type_id == TYPE_CONST_INT || type->type_id == TYPE_INT;
}

bool type_is_signed(Type *type)
{
    if (type->type_id == TYPE_CONST_INT) return true;
    return type->type_id == TYPE_INT && type->integer.is_signed;
}

uint64_t type_size(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
            return 0;
        case TYPE_VOID:
            return 1;
            return decl_size(type->decl);
        case TYPE_ENUM:
            return type_size(type->decl->enum_decl.type);
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
            // TODO pointer size
            return sizeof(void *);
        case TYPE_STRUCT:
        case TYPE_UNION:
            return decl_size(type->decl);
        case TYPE_NIL:
        case TYPE_STRING:
        case TYPE_POINTER:
            // TODO pointer size
            return sizeof(void *);
        case TYPE_ARRAY:
            // TODO check
            return type_size(type->array.base) * (type->array.is_empty ? 1 : type->array.len);
        case TYPE_OPAQUE:
            return type_size(type->opaque.base);
        case TYPE_UNRESOLVED:
            FATAL_ERROR("Should never happen");
        case TYPE_TYPEVAL:
            return type_size(type->type_of_type);
        case TYPE_FLOAT:
            return (uint64_t) ((type->float_bits + 7) / 8);
        case TYPE_BOOL:
            return 1;
        case TYPE_INT:
            return (uint64_t) ((type->integer.bits + 7) / 8);
        case TYPE_CONST_FLOAT:
            return 16; // TODO
        case TYPE_CONST_INT:
            return 8; // TODO
    }
    UNREACHABLE
}
void type_print_sub(const char *header, unsigned int current_indent, Type *type)
{
    if (!type) return;
    indent(current_indent);
    printf("%s %s\n", header, type_to_string(type));
}


/**
 * Convert value2 to value1 (note that we have already ordered things in conversion order.
 *
 * @param value1
 * @param value2
 * @return true if conversion worked.
 */
static bool value_convert_to_type_ordered(Value *value1, Value *value2)
{
    switch (value1->type)
    {
        case VALUE_TYPE_FLOAT:
            switch (value2->type)
            {
                case VALUE_TYPE_FLOAT:
                    value1->float_bits = value2->float_bits;
                    return true;
                case VALUE_TYPE_INT:
                    value_update_to_float(value2, bigint_as_float(&value2->big_int), value1->float_bits);
                    return true;
                case VALUE_TYPE_BOOL:
                    value_update_to_float(value2, value2->b ? 1.0 : 0.0, value1->float_bits);
                    return true;
                case VALUE_TYPE_NIL:
                    value_update_to_float(value2, 0.0, value1->float_bits);
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
            }
            UNREACHABLE
        case VALUE_TYPE_INT:
            switch (value2->type)
            {
                case VALUE_TYPE_INT:
                    // First check if we have a comptime int. If so, check that it fits.
                    if (value2->int_bits == 0)
                    {
                        if (value1->int_bits == 0) return true;
                        if (!bigint_fits_in_bits(&value2->big_int, value1->int_bits, !value1->is_unsigned)) return false;
                        BigInt res;
                        bigint_truncate(&res, &value2->big_int, value1->int_bits, !value1->is_unsigned);
                        value2->big_int = res;
                        return true;
                    }
                    if (!value1->is_unsigned && value2->is_unsigned)
                    {
                        // If unsigned value is same or larger, disallow!
                        if (value1->int_bits <= value2->int_bits) return false;

                        value2->is_unsigned = false;
                        value2->int_bits = value1->int_bits;
                        return true;
                    }
                    // Final case, both has same sign, promote to largest.
                    value2->int_bits = value1->int_bits;
                    return true;
                case VALUE_TYPE_BOOL:
                    bigint_init_unsigned(&value2->big_int, value2->b ? 1 : 0);
                    value2->int_bits = value1->int_bits;
                    value2->is_unsigned = value1->is_unsigned;
                    value2->type = VALUE_TYPE_INT;
                    return true;
                case VALUE_TYPE_NIL:
                    bigint_init_unsigned(&value2->big_int, 0);
                    value2->int_bits = value1->int_bits;
                    value2->is_unsigned = value1->is_unsigned;
                    value2->type = VALUE_TYPE_INT;
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
                case VALUE_TYPE_FLOAT:
                    UNREACHABLE;
            }
            UNREACHABLE;
        case VALUE_TYPE_BOOL:
            switch (value2->type)
            {
                case VALUE_TYPE_BOOL:
                    return true;
                case VALUE_TYPE_NIL:
                    value2->b = false;
                    value2->type = VALUE_TYPE_BOOL;
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
                case VALUE_TYPE_FLOAT:
                case VALUE_TYPE_INT:
                    UNREACHABLE;
            }
            UNREACHABLE;
        case VALUE_TYPE_NIL:
            switch (value2->type)
            {
                case VALUE_TYPE_NIL:
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
                case VALUE_TYPE_FLOAT:
                case VALUE_TYPE_BOOL:
                case VALUE_TYPE_INT:
                    UNREACHABLE;
            }
            UNREACHABLE;
        case VALUE_TYPE_STRING:
            return value2->type == VALUE_TYPE_STRING;
        case VALUE_TYPE_ERROR:
            return false;
    }
    UNREACHABLE;
}

bool decl_type_is_same(Decl *decl1, Decl *decl2)
{
    TODO;
    return false;
}

Type *type_unfold_opaque(Type *type)
{
    while (type->type_id == TYPE_OPAQUE)
    {
        type = type->opaque.base;
    }
    return type;
}

void type_copy(Type **dst, Type *source)
{
    TypeQualifier qualifier = (*dst)->qualifier;
    *dst = source;
    (*dst)->qualifier = qualifier;
}

bool type_may_convert_to_bool(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_TYPEVAL:
            return false;
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_CONST_FLOAT:break;
        case TYPE_CONST_INT:break;
        case TYPE_NIL:
            return true;
        default:
            TODO;
    }
    UNREACHABLE
}

bool type_is_same(Type *type1, Type *type2)
{
    type1 = type_unfold_opaque(type1);
    type2 = type_unfold_opaque(type2);
    if (type1 == type2) return true;
    if (type1->type_id != type2->type_id) return false;
    switch (type1->type_id)
    {
        case TYPE_INVALID:
        case TYPE_STRING:
        case TYPE_VOID:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_NIL:
        case TYPE_BOOL:
            return true;
        case TYPE_TYPEVAL:
            return type_is_same(type1->type_of_type, type2->type_of_type);
        case TYPE_INT:
            return type1->integer.is_signed == type2->integer.is_signed && type1->integer.bits == type2->integer.bits;
        case TYPE_FLOAT:
            return type1->float_bits == type2->float_bits;
        case TYPE_POINTER:
            return type_is_same(type1->pointer.base, type2->pointer.base);
        case TYPE_ARRAY:
            assert(type1->array.is_len_resolved && type2->array.is_len_resolved);
            return type_is_same(type1->array.base, type2->array.base) && type1->array.len == type2->array.len;
        case TYPE_UNION:
        case TYPE_ENUM:
        case TYPE_STRUCT:
        case TYPE_FUNC_TYPE:
        case TYPE_FUNC:
            return decl_type_is_same(type1->decl, type2->decl);
        case TYPE_OPAQUE:
            UNREACHABLE
        case TYPE_UNRESOLVED:
            FATAL_ERROR("Should be resolved at this point in time");
    }
}



static inline char *copy_constant_string(const char *string)
{
    char *res = malloc(strlen(string) + 1);
    strcpy(res, string);
    return res;
}

static inline char *get_embedded_type_name(const char *parent_name, Type *type)
{
    char *result = NULL;
    const char *sub_type = type_to_string(type);
    if (asprintf(&result, "%s[%s]", parent_name, sub_type) == -1)
    {
        return "ERROR";
    }
    return result;
}

static inline char *copy_token(Token *token)
{
    char *result = malloc(token->span.length + 1);
    strncpy(result, token->start, token->span.length);
    return result;
}

static inline const char *char_for_type(Type *type)
{
    char *result;
    switch (type->type_id)
    {
        case TYPE_INVALID:
            return "INVALID";
        case TYPE_UNRESOLVED:
            return "UNRESOLVED";
        case TYPE_VOID:
            return "void";
        case TYPE_STRING:
            return "string";
        case TYPE_OPAQUE:
            return get_embedded_type_name("opaque", type->opaque.base);
        case TYPE_POINTER:
            // const, restrict
            return get_embedded_type_name("ptr", type->pointer.base);
        case TYPE_ARRAY:
            // len
            return get_embedded_type_name("array", type->array.base);
        case TYPE_STRUCT:
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
        case TYPE_UNION:
        case TYPE_ENUM:
            return type->decl->name;
        case TYPE_TYPEVAL:
            return get_embedded_type_name("type", type->type_of_type);
        case TYPE_INT:
            if (type->integer.is_signed)
            {
                asprintf(&result, "u%d", type->integer.bits);
            }
            else
            {
                asprintf(&result, "i%d", type->integer.bits);
            }
            return result;
        case TYPE_BOOL:
            return "bool";
        case TYPE_FLOAT:
            asprintf(&result, "f%d", type->float_bits);
            return result;
        case TYPE_NIL:
            return "nil";
        case TYPE_CONST_FLOAT:
            return "const_float";
        case TYPE_CONST_INT:
            return "const_int";
    }
    UNREACHABLE
}

const char *type_to_string(Type *type)
{
    if (!type) return "NULL";
    if (!type->lazy_name) return type->lazy_name;
    return type->lazy_name = char_for_type(type);
}

bool type_order(Type *first, Type *second)
{
    if (first->type_id < second->type_id) return true;
    if (first->type_id > second->type_id) return false;
    switch (first->type_id)
    {
        case TYPE_FLOAT:
            return first->float_bits >= second->float_bits;
        case TYPE_INT:
            if (first->integer.bits > second->integer.bits) return true;
            if (first->integer.bits < second->integer.bits) return false;
            return !second->integer.is_signed && first->integer.is_signed;
        default:
            return true;
    }
}
