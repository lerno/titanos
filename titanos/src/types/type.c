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


TypeOld *type_unfold_redirects(TypeOld *type)
{
    while (1)
    {
        switch (type->type_id)
        {
            case XTYPE_RESOLVED:
                type = type->resolved;
                continue;
            case XTYPE_ALIAS:
                type = type->alias;
                continue;
            case XTYPE_OPAQUE:
                type = type->opaque;
                continue;
            default:
                return type;

        }
    }
}

QualifiedType type_canonical(QualifiedType type)
{
    QualifiedType canonical_type = type.type->canonical_type;
    if (canonical_type.type == NULL)
    {
        switch (type.type->type_id)
        {
            case TYPE_OPAQUE:
                canonical_type = type_canonical(type.type->base);
                break;
            case TYPE_ALIAS:
                canonical_type = type_canonical(type.type->decl->alias);
                break;
            case TYPE_POINTER:
                canonical_type = type_new_pointer(type_canonical(type.type->base));
                break;
            case TYPE_ARRAY:
                canonical_type = type_new_pointer(type_canonical(type.type->base));
                break;
            default:
                UNREACHABLE
        }
        type.type->canonical_type = canonical_type;
    }
    canonical_type.qualifier |= type.qualifier;
    return canonical_type;
}

QualifiedType type_remove_qualifier(QualifiedType qt)
{
    Type *type = qt.type;
    qt.qualifier = 0;
    switch (type->type_id)
    {
        case TYPE_OPAQUE:
            if (type->base.qualifier)
            {
                return type_remove_qualifier(type->base);
            }
            break;
        case TYPE_ALIAS:
            if (type->decl->alias.qualifier)
            {
                return type_remove_qualifier(type->decl->alias);
            }
            break;
        default:
            break;
    }
    return qt;
}

TypeOld *type_unfold_non_opaque(TypeOld *type)
{
    while (1)
    {
        switch (type->type_id)
        {
            case XTYPE_RESOLVED:
                type = type->resolved;
                continue;
            case XTYPE_ALIAS:
                type = type->alias;
                continue;
            default:
                return type;

        }
    }
}

Type *type_new(TypeId type_id)
{
    Type *type = malloc_arena(sizeof(Type));
    memset(type, 0, sizeof(TypeArray));
    type->type_id = type_id;
    return type;
}

TypeOld *new_unresolved_type(Expr *expr, bool public)
{
    assert(expr->expr_id == EXPR_TYPE || expr->expr_id == EXPR_IDENTIFIER || expr->expr_id == EXPR_ACCESS);
    TypeOld *type = new_type(XTYPE_UNRESOLVED, public);
    type->unresolved.type_expr = expr;
    return type;
}


TypeOld *new_type(TypeIdOld type_id, bool public)
{
    TypeOld *type = malloc_arena(sizeof(TypeOld));
    memset(type, 0, sizeof(TypeOld));

    type->type_id = type_id;
    type->is_public = public;

    return type;
}

QualifiedType type_new_pointer(QualifiedType base)
{
    Type *pointer = type_new(TYPE_POINTER);
    pointer->base = base;
    return (QualifiedType) { .type = pointer };
}


QualifiedType void_type()
{
    static Type type = { .type_id = TYPE_VOID, .canonical_type = { .type = &type }};
    return (QualifiedType) { .type = &type };
}

QualifiedType type_nil()
{
    static Type type = { .type_id = TYPE_NIL, .canonical_type = { .type = &type }};
    return (QualifiedType) { .type = &type };
}

QualifiedType type_string()
{
    static Type type = { .type_id = TYPE_STRING };
    return (QualifiedType) { .type = &type };
}

QualifiedType type_compint()
{
    static Type type = { .type_id = TYPE_CONST_INT, .canonical_type.type = &type };
    return (QualifiedType) { .type = &type };
}

QualifiedType type_compfloat()
{
    static Type type = { .type_id = TYPE_CONST_FLOAT, .canonical_type.type = &type };
    return (QualifiedType) { .type = &type };
}

bool type_is_signed2(TypeOld *type)
{
    type = type_unfold_redirects(type);
    if (type->type_id == XTYPE_CONST_INT) return true;
    return type->type_id == XTYPE_INT && type->integer.is_signed;
}

bool type_is_float(Type *type)
{
    if (type->type_id == TYPE_CONST_FLOAT) return true;
    return type->type_id >= MAX_FLOAT_TYPE && type->type_id <= MIN_FLOAT_TYPE;
}



uint64_t type_size(TypeOld *type)
{
    type = type_unfold_redirects(type);
    switch (type->type_id)
    {
        case XTYPE_INVALID:
            return 0;
        case XTYPE_VOID:
            return 1;
            return decl_size(type->decl);
        case XTYPE_ENUM:
            return type_size(type->decl->enum_decl.type.type);
        case XTYPE_FUNC:
        case XTYPE_FUNC_TYPE:
            // TODO pointer size
            return sizeof(void *);
        case XTYPE_STRUCT:
        case XTYPE_UNION:
            return decl_size(type->decl);
        case XTYPE_NIL:
        case XTYPE_STRING:
        case XTYPE_POINTER:
            // TODO pointer size
            return sizeof(void *);
        case XTYPE_ARRAY:
            // TODO check
            return type_size(type->array.base) * (type->array.is_empty ? 1 : type->array.len);
        case XTYPE_UNRESOLVED:
            FATAL_ERROR("Should never happen");
        case XTYPE_TYPEVAL:
            return type_size(type->type_of_type);
        case XTYPE_FLOAT:
            return (uint64_t) ((type->float_bits + 7) / 8);
        case XTYPE_BOOL:
            return 1;
        case XTYPE_INT:
            return (uint64_t) ((type->integer.bits + 7) / 8);
        case XTYPE_CONST_FLOAT:
            return 16; // TODO
        case XTYPE_CONST_INT:
            return 8; // TODO
        case XTYPE_RESOLVED:
        case XTYPE_OPAQUE:
        case XTYPE_ALIAS:
            UNREACHABLE
    }
    UNREACHABLE
}

void type_print_sub(const char *header, unsigned int current_indent, TypeOld *type)
{
    if (!type) return;
    indent(current_indent);
    printf("%s %s\n", header, type_to_string2(type));
}

void type_print(QualifiedType type)
{
    if (IS_INVALID(type))
    {
        printf("<invalid>");
        return;;
    }
    if (IS_CONST(type)) printf("const ");
    if (IS_VOLATILE(type)) printf("volatile ");
    if (IS_ALIAS(type)) printf("alias ");
    switch (type.type->type_id)
    {
        case TYPE_VOID:
            printf("void");
            break;
        case TYPE_OPAQUE:
            printf("opaque(");
            type_print(type.type->base);
            printf(")");
            break;
        case TYPE_ALIAS:
            printf("%s->", type.type->decl->name);
            type_print(type.type->decl->alias);
            break;
        case TYPE_POINTER:
            type_print(type.type->base);
            printf("*");
            break;
        case TYPE_CONST_FLOAT:
            printf("const_float");
            break;
        case TYPE_CONST_INT:
            printf("const_int");
            break;
        case TYPE_NIL:
            printf("nil");
            break;
        case TYPE_STRING:
            printf("string");
            break;
        case TYPE_ARRAY:
            type_print(type.type->base);
            printf("[%d]", type.type->len);
        default:
            printf("%s", type.type->decl->name);
            break;
    }
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
                        if (!bigint_fits_in_bits(&value2->big_int,
                                                 value1->int_bits,
                                                 !value1->is_unsigned))
                        {
                            return false;
                        }
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

void type_copy(TypeOld **dst, TypeOld *source)
{
    TypeQualifier qualifier = (*dst)->qualifier;
    *dst = source;
    (*dst)->qualifier = qualifier;
}

bool type_may_convert_to_bool(TypeOld *type)
{
    switch (type->type_id)
    {
        case XTYPE_INVALID:
        case XTYPE_UNRESOLVED:
        case XTYPE_VOID:
        case XTYPE_STRING:
        case XTYPE_OPAQUE:
        case XTYPE_TYPEVAL:
            return false;
        case XTYPE_POINTER:
        case XTYPE_ARRAY:
        case XTYPE_CONST_FLOAT:
            break;
        case XTYPE_CONST_INT:
            break;
        case XTYPE_NIL:
            return true;
        default:
            TODO;
    }
    UNREACHABLE
}


bool type_is_aritmetic(Type *canonical_type)
{
    switch (canonical_type->type_id)
    {
        case TYPE_OPAQUE:
        case TYPE_ALIAS:
            UNREACHABLE
        case TYPE_POINTER:
        case TYPE_STRING:
        case TYPE_ARRAY:
        case TYPE_FUNC_TYPE:
        case TYPE_FUNC:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_VOID:
            return false;
        case TYPE_F64:
        case TYPE_F32:
        case TYPE_U64:
        case TYPE_I64:
        case TYPE_U32:
        case TYPE_I32:
        case TYPE_U16:
        case TYPE_I16:
        case TYPE_U8:
        case TYPE_I8:
        case TYPE_ENUM:
        case TYPE_BOOL:
        case TYPE_NIL:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
            return true;
    }
    UNREACHABLE
}

bool type_is_same(Type *type1, Type *type2)
{
    QualifiedType c_type1 = type_canonical(type1->canonical_type);
    QualifiedType c_type2 = type_canonical(type2->canonical_type);
    if (c_type1.qualifier != c_type2.qualifier) return false;
    if (c_type1.type->type_id < c_type2.type->type_id)
    {
        type1 = c_type1.type;
        type2 = c_type2.type;
    }
    else
    {
        type1 = c_type2.type;
        type2 = c_type1.type;
    }

    if (type1 == type2) return true;
    if (type1->type_id != type2->type_id) return false;
    switch (type1->type_id)
    {
        case TYPE_POINTER:
            c_type1 = type1->base;
            c_type2 = type2->base;
            if (c_type1.qualifier != c_type2.qualifier) return false;
            return type_is_same(c_type1.type, c_type2.type);
        case TYPE_ARRAY:
            c_type1 = type1->base;
            c_type2 = type2->base;
            if (c_type1.qualifier != c_type2.qualifier) return false;
            if (!type_is_same(c_type1.type, c_type2.type)) return false;
            return c_type1.type->len == c_type2.type->len;
        case TYPE_FUNC_TYPE:
            TODO
        case TYPE_ENUM:
        case TYPE_FUNC:
        case TYPE_STRUCT:
        case TYPE_UNION:
            // Should always be based on the decl, so types should be identical
            UNREACHABLE
        case TYPE_VOID:
        case TYPE_F64:
        case TYPE_F32:
        case TYPE_U64:
        case TYPE_I64:
        case TYPE_U32:
        case TYPE_I32:
        case TYPE_U16:
        case TYPE_I16:
        case TYPE_U8:
        case TYPE_I8:
        case TYPE_BOOL:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_STRING:
        case TYPE_NIL:
            // Should be singleton
            UNREACHABLE
        case TYPE_OPAQUE:
        case TYPE_ALIAS:
            // Not canonical
            UNREACHABLE
    }
}

static inline char *copy_constant_string(const char *string)
{
    char *res = malloc(strlen(string) + 1);
    strcpy(res, string);
    return res;
}

static inline char *get_embedded_type_name(const char *parent_name, QualifiedType type)
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
    switch (type->type_id)
    {
        case TYPE_VOID:
            return "void";
        case TYPE_STRING:
            return "string";
        case TYPE_OPAQUE:
            return get_embedded_type_name("opaque", type->base);
        case TYPE_POINTER:
            // const, restrict
            return get_embedded_type_name("ptr", type->base);
        case TYPE_ARRAY:
            // len
            return get_embedded_type_name("array", type->base);
        case TYPE_NIL:
            return "nil";
        case TYPE_CONST_FLOAT:
            return "const_float";
        case TYPE_CONST_INT:
            return "const_int";
        case TYPE_ALIAS:
            return get_embedded_type_name("alias", type->decl->alias);
        case TYPE_F64:
            return "f64";
        case TYPE_F32:
            return "f32";
        case TYPE_U64:
            return "u64";
        case TYPE_U32:
            return "u32";
        case TYPE_U16:
            return "u16";
        case TYPE_U8:
            return "i8";
        case TYPE_I64:
            return "i64";
        case TYPE_I32:
            return "i32";
        case TYPE_I16:
            return "i16";
        case TYPE_I8:
            return "i8";
        case TYPE_BOOL:
            return "bool";
        case TYPE_FUNC_TYPE:
        case TYPE_ENUM:
        case TYPE_FUNC:
        case TYPE_STRUCT:
        case TYPE_UNION:
            return type->decl->name;
    }
    UNREACHABLE
}

const char *type_to_string2(TypeOld *type)
{
    TODO;
}

const char *type_to_string(QualifiedType type)
{
    if (IS_INVALID(type)) return "<invalid>";
    const char *templ;
    switch (type.qualifier)
    {
        case TYPE_QUALIFIER_NONE:
            templ = NULL;
            break;
        case TYPE_QUALIFIER_CONST:
            templ = "const %s";
            break;
        case TYPE_QUALIFIER_ALIAS:
            templ = "alias %s";
            break;
        case TYPE_QUALIFIER_VOLATILE:
            templ = "volatile %s";
            break;
        case TYPE_QUALIFIER_ALIAS | TYPE_QUALIFIER_CONST:
            templ = "const alias %s";
            break;
        case TYPE_QUALIFIER_VOLATILE | TYPE_QUALIFIER_CONST:
            templ = "const volatile %s";
            break;
        case TYPE_QUALIFIER_VOLATILE | TYPE_QUALIFIER_ALIAS:
            templ = "volatile alias %s";
            break;
        case TYPE_QUALIFIER_VOLATILE | TYPE_QUALIFIER_CONST | TYPE_QUALIFIER_ALIAS:
            templ = "const volatile alias %s";
            break;
        default:
            templ = "? %s";
            break;
    }
    const char *name = char_for_type(type.type);
    if (!templ) return name;
    char *name_with_q;
    asprintf(&name_with_q, templ, name);
    return name_with_q;
}

bool type_order(TypeOld *first, TypeOld *second)
{
    if (first->type_id < second->type_id) return true;
    if (first->type_id > second->type_id) return false;
    switch (first->type_id)
    {
        case XTYPE_FLOAT:
            return first->float_bits >= second->float_bits;
        case XTYPE_INT:
            if (first->integer.bits > second->integer.bits) return true;
            if (first->integer.bits < second->integer.bits) return false;
            return !second->integer.is_signed && first->integer.is_signed;
        default:
            return true;
    }
}

QualifiedType InvalidType = { .type = NULL };
