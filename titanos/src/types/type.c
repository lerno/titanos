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
    Type *type = new_type(TYPE_UNRESOLVED, public, &expr->span);
    type->unresolved.type_expr = expr;
    return type;
}

Type *new_type(TypeId type_id, bool public, Token *initial_token)
{
    Type *type = malloc_arena(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->type_id = type_id;
    type->span = *initial_token;
    type->is_public = public;

    return type;
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
    if (type->type_id == TYPE_CONST_INT) return true;
    return type->type_id == TYPE_BUILTIN &&
           (type->builtin.builtin_kind == BUILTIN_UNSIGNED_INT || type->builtin.builtin_kind == BUILTIN_SIGNED_INT);
}

bool type_is_signed(Type *type)
{
    if (type->type_id == TYPE_CONST_INT) return true;
    return type->type_id == TYPE_BUILTIN && type->builtin.builtin_kind == BUILTIN_SIGNED_INT;
}

uint64_t type_size(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
            return 0;
        case TYPE_VOID:
            return 1;
        case TYPE_DECLARED:
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
        case TYPE_IMPORT:
            return 0;
        case TYPE_UNRESOLVED:
            FATAL_ERROR("Should never happen");
        case TYPE_TYPEVAL:
            return type_size(type->type_of_type);
        case TYPE_BUILTIN:
            return (uint64_t) ((type->builtin.bits + 7) / 8);
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
    printf("%s:\n", header);
    print_type(type, current_indent + 1);
}

static void print_unresolved_or_name(const char *name, Type *type)
{
    printf("%s ", name);
    if (!type->decl)
    {
        printf("[UNRESOLVED]\n");
    }
    else
    {
        print_token(&type->decl->name);
        printf("\n");
    }
}
void print_type(Type *type, unsigned current_indent)
{
    indent(current_indent++);
    if (!type)
    {
        printf("[NULL]\n");
        return;
    }
    switch (type->type_id)
    {
        case TYPE_VOID:
            printf("TYPE_VOID\n");
            return;
        case TYPE_DECLARED:
            printf("TYPE_DECLARED\n");
            return;
        case TYPE_POINTER:
            print_unresolved_or_name("TYPE_POINTER", type);
            type_print_sub("Opaque", current_indent, type->pointer.base);
            return;
        case TYPE_ARRAY:
            print_unresolved_or_name("TYPE_ARRAY", type);
            // Improve add length
            return;
        case TYPE_NIL:
            printf("TYPE_NIL\n");
            return;
        case TYPE_OPAQUE:
            print_unresolved_or_name("TYPE_OPAQUE", type);
            type_print_sub("Opaque", current_indent, type->opaque.base);
            return;
        case TYPE_IMPORT:
            print_unresolved_or_name("TYPE_IMPORT", type);
            return;
        case TYPE_UNRESOLVED:
            printf("UNRESOLVED\n");
            expr_print_sub("Ast", current_indent, type->unresolved.type_expr);
            return;
        case TYPE_INVALID:
            printf("TYPE_INVALID\n");
            return;
        case TYPE_TYPEVAL:
            printf("TYPEVAL\n");
            type_print_sub("Type", current_indent, type->type_of_type);
            return;
        case TYPE_BUILTIN:
            printf("BUILTIN %.*s\n", SPLAT_TOK(type->name));
            return;
        case TYPE_STRING:
            printf("BUILTIN STRING\n");
            return;
        case TYPE_CONST_FLOAT:
            printf("CONT_FLOAT\n");
            return;
        case TYPE_CONST_INT:
            printf("CONST_INT\n");
            return;
    }
    UNREACHABLE
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

static inline Type *unfold_opaque(Type *type)
{
    while (type->type_id == TYPE_OPAQUE)
    {
        type = type->opaque.base;
    }
    return type;
}
bool type_is_same(Type *type1, Type *type2)
{
    type1 = unfold_opaque(type1);
    type2 = unfold_opaque(type2);
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
            return true;
        case TYPE_TYPEVAL:
            return type_is_same(type1->type_of_type, type2->type_of_type);
            break;
        case TYPE_BUILTIN:
            return type1->builtin.builtin_kind == type2->builtin.builtin_kind
                   && type1->builtin.bits == type2->builtin.bits;
        case TYPE_POINTER:
            return type_is_same(type1->pointer.base, type2->pointer.base);
        case TYPE_ARRAY:
            assert(type1->array.is_len_resolved && type2->array.is_len_resolved);
            return type_is_same(type1->array.base, type2->array.base) && type1->array.len == type2->array.len;
        case TYPE_DECLARED:
            return decl_type_is_same(type1->decl, type2->decl);
        case TYPE_OPAQUE:
        case TYPE_IMPORT:
            UNREACHABLE
        case TYPE_UNRESOLVED:
            FATAL_ERROR("Should be resolved at this point in time");
    }
}


Type *type_implicit_convert_ordered(Expr *where, Type *type1, Type *type2)
{
    int a[4];
    int *b;
    int *c = a ;

    switch (type1->type_id)
    {
        case TYPE_INVALID:
            // Invalid always yield invalid
            return NULL;
        case TYPE_UNRESOLVED:
            FATAL_ERROR("Types should already be resolved");
        case TYPE_IMPORT:
            UNREACHABLE
        case TYPE_VOID:
            sema_error_at(&where->span, "Other types cannot be converted to void");
            return NULL;
        case TYPE_STRING:
            sema_error_at(&where->span, "Cannot use string in expression");
            return NULL;
        case TYPE_OPAQUE:
            sema_error_at(&where->span, "Cannot implicitly convert opaque types");
            return NULL;
        case TYPE_POINTER:
        case TYPE_ARRAY:
            switch (type2->type_id)
            {
                case TYPE_INVALID:
                case TYPE_UNRESOLVED:
                case TYPE_IMPORT:
                case TYPE_VOID:
                case TYPE_STRING:
                case TYPE_OPAQUE:
                    UNREACHABLE;
                case TYPE_POINTER:
                    sema_error_at(&where->span, "Cannot implicitly convert one pointer type to the other");
                    return NULL;
                case TYPE_ARRAY:
                    if (!type_is_same(type1->pointer.base, type2->array.base)) return NULL;
                    sema_error_at(&where->span, "Cannot implicitly convert one pointer type to the other");
                    return type1;
                case TYPE_DECLARED:
                    sema_error_at(&where->span, "Cannot implicitly convert the types");
                    return NULL;
                case TYPE_TYPEVAL:
                    sema_error_at(&where->span, "Cannot convert a type into a pointer");
                    return NULL;
                case TYPE_BUILTIN:
                    switch (type2->builtin.builtin_kind)
                    {
                        case BUILTIN_SIGNED_INT:
                        case BUILTIN_UNSIGNED_INT:
                            return type1;
                        case BUILTIN_FLOAT:
                            sema_error_at(&where->span, "Cannot convert a float into a pointer offset");
                            return NULL;
                        case BUILTIN_BOOL:
                            sema_error_at(&where->span, "Cannot convert a bool into a pointer offset");
                            return NULL;
                    }
                    UNREACHABLE
                case TYPE_CONST_FLOAT:
                    sema_error_at(&where->span, "Cannot convert a float into a pointer offset");
                    return NULL;
                case TYPE_CONST_INT:
                case TYPE_NIL:
                    return type1;
            }
            UNREACHABLE
        case TYPE_DECLARED:
            sema_error_at(&where->span, "Cannot convert the types");
            return NULL;
        case TYPE_TYPEVAL:
            sema_error_at(&where->span, "Cannot convert the types");
            return NULL;
        case TYPE_BUILTIN:
            switch (type2->type_id)
            {
                case TYPE_INVALID:
                case TYPE_UNRESOLVED:
                case TYPE_IMPORT:
                case TYPE_VOID:
                case TYPE_STRING:
                case TYPE_OPAQUE:
                case TYPE_POINTER:
                case TYPE_ARRAY:
                case TYPE_DECLARED:
                case TYPE_TYPEVAL:
                    UNREACHABLE
                case TYPE_BUILTIN:
                    switch (type2->builtin.builtin_kind)
                    {
                        case BUILTIN_SIGNED_INT:
                        case BUILTIN_UNSIGNED_INT:
                            return type1;
                        case BUILTIN_FLOAT:
                            sema_error_at(&where->span, "Cannot convert a float into a pointer offset");
                            return NULL;
                        case BUILTIN_BOOL:
                            sema_error_at(&where->span, "Cannot convert a bool into a pointer offset");
                            return NULL;
                    }
                    UNREACHABLE
                case TYPE_CONST_FLOAT:
                    TODO
                    sema_error_at(&where->span, "Cannot convert a float into a pointer offset");
                    return NULL;
                case TYPE_CONST_INT:
                case TYPE_NIL:
                    return type1;
            }
            UNREACHABLE
            return type1;
        case TYPE_CONST_FLOAT:
            break;
        case TYPE_CONST_INT:break;
        case TYPE_NIL:break;
    }
    return NULL;
}
Type *type_implicit_convert(Expr *expr, Type *type1, Type *type2)
{
    if (type_is_same(type1, type2)) return type1;
    bool reverse_order = false;
    if (type1->type_id > type2->type_id)
    {
        reverse_order = true;
    }
    else if (type2->type_id == type1->type_id && type1->type_id == TYPE_BUILTIN)
    {
        int bit_diff = (int)type1->builtin.bits - (int)type2->builtin.bits;
        switch (type1->builtin.builtin_kind)
        {
            case BUILTIN_FLOAT:
                reverse_order = type2->builtin.builtin_kind == BUILTIN_FLOAT && bit_diff < 0;
                break;
            case BUILTIN_UNSIGNED_INT:
                reverse_order = type2->builtin.builtin_kind == BUILTIN_FLOAT || bit_diff < 0;
                break;
            case BUILTIN_SIGNED_INT:
                reverse_order = type2->builtin.builtin_kind == BUILTIN_FLOAT || bit_diff <= 0;
                break;
            case BUILTIN_BOOL:
                reverse_order = type2->builtin.builtin_kind != BUILTIN_BOOL;
                break;
        }
    }
    if (reverse_order) return type_implicit_convert_ordered(expr, type2, type1);
    return type_implicit_convert_ordered(expr, type1, type2);
}

/*
 *             printf("TYPE_DEFINITION ");
print_token(&ast->definition.name);
if (ast->definition.is_public) printf(" public");
if (ast->definition.is_exported) printf(" exported");
switch (ast->definition.definition_type)
{
    case STRUCT_TYPE:
        printf(ast->definition.is_struct ? " STRUCT" : " UNION");
        printf("\n");
        print_sub_ast_list("Members", current_indent, ast->definition.def_struct.members);
        break;
    case ENUM_TYPE:
        printf(" enum");
        if (ast->definition.is_incremental) printf(" incr");
        printf("\n");
        print_sub_ast("Type", current_indent, ast->definition.def_enum.type);
        print_sub_ast_list("Entries", current_indent, ast->definition.def_enum.entries);
        break;
    case ENUM_ENTRY_TYPE:
        printf(" enum entry");
        printf("\n");
        print_sub_ast("Value", current_indent, ast->definition.def_enum_entry.value);
        break;
    case ALIAS_TYPE:
        printf(" alias\n");
        print_sub_ast("Type", current_indent, ast->definition.def_alias.type_definition);
        break;
    case FUNC_TYPE:
        printf(" func\n");
        print_sub_ast("Declaration", current_indent, ast->definition.def_func.func_decl);
        printf("BUILTIN_TYPE ");
        break;
    case BUILTIN_TYPE:
        switch (ast->definition.def_builtin.type)
        {
            case BUILTIN_BOOL:
                printf(" bool\n");
                break;
            case BUILTIN_FLOAT:
                printf(" float %d bits\n", ast->definition.def_builtin.bits);
                break;
            case BUILTIN_INT:
                printf(" int %d bits\n", ast->definition.def_builtin.bits);
                break;
            case BUILTIN_UINT:
                printf(" uint %d bits\n", ast->definition.def_builtin.bits);
                break;
        }
        break;

}
print_sub_ast("Attributes", current_indent, ast->definition.attributes);

 */

/*        case AST_DECL:
            printf("DECLARATION ");
            print_token(&type->decl.name);
            if (type->decl.is_parameter) printf(" parameter");
            if (type->decl.is_used) printf(" used");
            if (type->decl.is_public) printf(" public");
            if (type->decl.is_exported) printf(" exported");
            printf("\n");
            print_sub_ast("Type", current_indent, type->decl.type);
            print_sub_ast("Init", current_indent, type->decl.init_expr);
            return;
        case AST_STRUCT_MEMBER:
            printf("STRUCT_MEMBER ");
            if (type->struct_member.name.length) print_token(&type->struct_member.name);
            printf("\n");
            switch (type->struct_member.type)
            {
                case STRUCT_MEMBER_TYPE_NORMAL:
                    print_sub_ast("Value", current_indent, type->struct_member.value_type);
                    break;
                case STRUCT_MEMBER_TYPE_UNION:
                    print_sub_ast_list("Union", current_indent, type->struct_member.members);
                    break;
                case STRUCT_MEMBER_TYPE_STRUCT:
                    print_sub_ast_list("Struct", current_indent, type->struct_member.members);
                    break;
            }
            return;
        case AST_ATTRIBUTE_LIST:
            printf("ATTRIBUTE_LIST\n");
            print_sub_ast_list("Attrs", current_indent, type->attribute_list.list);
            return;
        case AST_ATTRIBUTE:
            printf("ATTRIBUTE ");
            print_token(&type->attribute.name);
            print_sub_ast("Value", current_indent, type->attribute.value);
            return;
        case AST_TYPE_EXPR:
            printf("TYPE_EXPR");
            if (type->type_expr.flags.local) printf(" local");
            if (type->type_expr.flags.const_ref) printf(" const");
            if (type->type_expr.flags.alias_ref) printf(" alias");
            if (type->type_expr.flags.volatile_ref) printf(" volatile");
            switch (type->type_expr.type)
            {
                case TYPE_EXPR_VOID:
                    printf(" VOID\n");
                    break;
                case TYPE_EXPR_POINTER:
                    printf(" POINTER\n");
                    print_sub_ast("Type", current_indent, type->type_expr.pointer_type_expr.type);
                    break;
                case TYPE_EXPR_ARRAY:
                    printf(" ARRAY\n");
                    print_sub_ast("Base", current_indent, type->type_expr.array_type_expr.type);
                    if (type->type_expr.flags.resolved)
                    {
                        indent(current_indent);
                        printf("Size: %llu\n", type->type_expr.array_type_expr.fix_size);
                    }
                    else if (type->type_expr.array_type_expr.size)
                    {
                        print_sub_ast("Size", current_indent, type->type_expr.array_type_expr.size);
                    }
                    break;
                case TYPE_EXPR_IDENTIFIER:
                    printf(" IDENTIFIER ");
                    if (type->type_expr.identifier_type_expr.module_name.length)
                    {
                        print_token(&type->type_expr.identifier_type_expr.module_name);
                        printf(".");
                    }
                    print_token(&type->type_expr.identifier_type_expr.name);
                    if (!type->type_expr.identifier_type_expr.resolved_type)
                    {
                        printf(" [UNRESOLVED]");
                    }
                    printf("\n");
                    print_sub_ast("Type", current_indent, type->type_expr.identifier_type_expr.resolved_type);
                    break;
                default:
                    printf("Unknown!\n");
            }
            return;
        case AST_STRUCT_INIT_VALUES_EXPR:
            printf("STRUCT_INIT_VALUES_EXPR\n");
            print_sub_ast_list("Values", current_indent, type->struct_init_values_expr.values);
            return;
        case AST_DESIGNATED_INITIALIZED_EXPR:
            printf("DESIGNATED_INITIALIZED_EXPR ");
            print_token(&type->designated_initializer_expr.identifer);
            printf("\n");
            print_sub_ast("Value", current_indent, type->designated_initializer_expr.expr);
            return;
        case AST_TYPE_DEFINITION:
            printf("TYPE_DEFINITION ");
            print_token(&type->definition.name);
            if (type->definition.is_public) printf(" public");
            if (type->definition.is_exported) printf(" exported");
            switch (type->definition.definition_type)
            {
                case STRUCT_TYPE:
                    printf(type->definition.is_struct ? " STRUCT" : " UNION");
                    printf("\n");
                    print_sub_ast_list("Members", current_indent, type->definition.def_struct.members);
                    break;
                case ENUM_TYPE:
                    printf(" enum");
                    if (type->definition.is_incremental) printf(" incr");
                    printf("\n");
                    print_sub_ast("Type", current_indent, type->definition.def_enum.type);
                    print_sub_ast_list("Entries", current_indent, type->definition.def_enum.entries);
                    break;
                case ENUM_ENTRY_TYPE:
                    printf(" enum entry");
                    printf("\n");
                    print_sub_ast("Value", current_indent, type->definition.def_enum_entry.value);
                    break;
                case ALIAS_TYPE:
                    printf(" alias\n");
                    print_sub_ast("Type", current_indent, type->definition.def_alias.type_definition);
                    break;
                case FUNC_TYPE:
                    printf(" func\n");
                    print_sub_ast("Declaration", current_indent, type->definition.def_func.func_decl);
                    printf("BUILTIN_TYPE ");
                    break;
                case BUILTIN_TYPE:
                    switch (type->definition.def_builtin.type)
                    {
                        case BUILTIN_BOOL:
                            printf(" bool\n");
                            break;
                        case BUILTIN_FLOAT:
                            printf(" float %d bits\n", type->definition.def_builtin.bits);
                            break;
                        case BUILTIN_INT:
                            printf(" int %d bits\n", type->definition.def_builtin.bits);
                            break;
                        case BUILTIN_UINT:
                            printf(" uint %d bits\n", type->definition.def_builtin.bits);
                            break;
                    }
                    break;

            }
            print_sub_ast("Attributes", current_indent, type->definition.attributes);
            return;
        case AST_VAR_DEFINITION:
            printf("VAR_DEFINITION ");
            print_token(&type->var_definition.name);
            if (type->var_definition.is_public) printf(" public");
            if (type->var_definition.is_exported) printf(" exported");
            printf("\n");
            print_sub_ast("Type", current_indent, type->var_definition.type);
            print_sub_ast("Value", current_indent, type->var_definition.value);
            print_sub_ast("Attributes", current_indent, type->var_definition.attributes);
            return;
        case AST_INCREMENTAL_ARRAY:
            printf("INCREMENTAL_ARRAY ");
            print_token(&type->incremental_array.name);
            print_sub_ast("Value", current_indent, type->incremental_array.value);
            return;
        case AST_SIZEOF_EXPR:
            printf("SIZEOF_EXPR\n");
            print_sub_ast("Expr", current_indent, type->sizeof_expr.expr);
            return;
        case AST_CAST_EXPR:
            printf("CAST_EXPR\n");
            print_sub_ast("Expr", current_indent, type->cast_expr.expr);
            print_sub_ast("Type", current_indent, type->cast_expr.type);
            return;
        case AST_DEFER_RELASE:
            printf("DEFER_RELEASE\n");
            print_sub_ast("Inner", current_indent, type->defer_release_stmt.inner);
            print_sub_ast("DeferStart", current_indent, type->defer_release_stmt.list.defer_start);
            print_sub_ast("DeferEnd", current_indent, type->defer_release_stmt.list.defer_end);
            return;*/
