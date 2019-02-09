//
// Created by Christoffer Lernö on 2019-02-03.
//

#include <string.h>
#include <printer.h>
#include <expr.h>
#include "type.h"
#include "arena_allocator.h"
#include "ast_types.h"
#include "decl.h"

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

Type *end_type(Type *type, Token *end)
{
    token_expand(&type->span, end);
    return type;
}

Type *void_type()
{
    static Type type = { .type_id = TYPE_VOID, .is_public = true };
    return &type;
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
        case TYPE_FUNC:
            print_unresolved_or_name("TYPE_FUNC", type);
            return;
        case TYPE_ENUM:
            print_unresolved_or_name("TYPE_ENUM", type);
            return;
        case TYPE_VOID:
            printf("TYPE_VOID\n");
            return;
        case TYPE_BOOL:
            printf("TYPE_BOOL\n");
            return;
        case TYPE_INT:
            printf("TYPE_INT %s-%d\n", type->integer.is_signed ? "signed" : "unsigned", type->integer.bits);
            return;
        case TYPE_FLOAT:
            printf("TYPE_FLOAT [%d]\n", type->real.bits);
            return;
        case TYPE_POINTER:
            print_unresolved_or_name("TYPE_POINTER", type);
            type_print_sub("Opaque", current_indent, type->pointer.base);
            return;
        case TYPE_ARRAY:
            print_unresolved_or_name("TYPE_ARRAY", type);
            // Improve add length
            return;
        case TYPE_STRUCT:
            print_unresolved_or_name("TYPE_STRUCT", type);
            return;
        case TYPE_CONST_FLOAT:
            printf("TYPE_CONST_FLOAT\n");
            return;
        case TYPE_CONST_INT:
            printf("TYPE_CONST_INT\n");
            return;
        case TYPE_NIL:
            printf("TYPE_NIL\n");
            return;
        case TYPE_UNION:
            print_unresolved_or_name("TYPE_UNION", type);
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
            break;
        case TYPE_INVALID:
            printf("TYPE_INVALID\n");
            return;
        case TYPE_TYPEVAL:
            printf("TYPEVAL\n");
            type_print_sub("Type", current_indent, type->type_of_type);
            break;
    }
    printf("TODO\n");
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
