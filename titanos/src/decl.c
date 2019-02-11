//
// Created by Christoffer Lernö on 2019-02-04.
//

#include <string.h>
#include "decl.h"
#include "arena_allocator.h"
#include "printer.h"
#include "attributes.h"
#include "ast_utils.h"
#include "expr.h"

void decl_init(Decl *decl, DeclType decl_type, Token *span, Token *name, bool public)
{
    memset(decl, 0, sizeof(Decl));
    decl->type_id = decl_type;
    decl->name = *name;
    decl->span = *span;
    decl->is_public = public;
    decl->type.decl = decl;
    decl->type.span = *span;
    decl->type.type_id = TYPE_DECLARED;
}
Decl *decl_new(DeclType decl_type, Token *span, Token *name, bool public)
{
    Decl *decl = malloc_arena(sizeof(Decl));
    decl_init(decl, decl_type, span, name, public);
    return decl;
}

void decl_print_name_visibility(Decl *decl)
{
    print_token(&decl->name);
    if (decl->is_public) printf(" public");
    if (decl->is_exported) printf(" exported");
    if (decl->is_used_public) printf(" public-use");
    if (decl->is_used) printf(" used");
    if (decl->has_cname) printf(" cnamed");
}

void decl_print_sub(const char *header, unsigned current_indent, Decl *decl)
{
    if (!decl) return;
    indent(current_indent);
    printf("%s:\n", header);
    decl_print(decl, current_indent + 1);
}

static void decl_print_sub_list(const char *header, unsigned current_indent, Vector *list)
{
    if (list->size == 0)
    {
        indent(current_indent);
        printf("No '%s' entries\n", header);
        return;
    }
    indent(current_indent);
    printf("%s:\n", header);

    for (int i = 0; i < list->size; i++)
    {
        char buffer[100];
        sprintf(buffer, "[%d]", i);
        decl_print_sub(buffer, current_indent + 1, list->entries[i]);
    }
}

void decl_print(Decl *decl, unsigned current_indent)
{
    indent(current_indent);
    switch (decl->type_id)
    {
        case DECL_IMPORT:
            printf("IMPORT ");
            print_token(&decl->name);
            switch (decl->import.type)
            {
                case IMPORT_TYPE_FULL:
                    break;
                case IMPORT_TYPE_ALIAS:
                    printf(" alias ");
                    print_token(&decl->import.alias);
                    break;
                case IMPORT_TYPE_LOCAL:
                    printf(" local");
                    break;
            }
            printf("\n");
            return;
        case DECL_FUNC:
            printf("FUNC ");
            decl_print_name_visibility(decl);
            if (decl->func_decl.variadic) printf(" variadic");
            if (decl->func_decl.is_struct_func) printf(" struct func");
            printf("\n");
            print_sub_ast("Body", current_indent, decl->func_decl.body);
            decl_print_sub_list("Params", current_indent, decl->func_decl.args);
            return;
        case DECL_VAR:
            printf("VAR ");
            decl_print_name_visibility(decl);
            printf("\n");

            // print_sub_ast("Type", current_indent, ast->decl.type);
            expr_print_sub("Init", current_indent, decl->var.init_expr);
            return;
        case DECL_ENUM_CONSTANT:
            printf("ENUM_CONSTANT ");
            decl_print_name_visibility(decl);
            printf("\n");
            expr_print_sub("Value", current_indent, decl->enum_constant.init_value);
            return;
        case DECL_ALIAS_TYPE:
            printf("ALIAS_TYPE ");
            decl_print_name_visibility(decl);
            printf("\n");
            type_print_sub("Alias", current_indent, decl->alias_decl.type);
            return;
        case DECL_STRUCT_TYPE:
            printf("STRUCT_TYPE ");
            decl_print_name_visibility(decl);
            printf("\n");
            printf("TODO\n");
            return;
        case DECL_ENUM_TYPE:
            printf("ENUM_TYPE ");
            decl_print_name_visibility(decl);
            printf("\n");
            printf("TODO\n");
            return;
        case DECL_FUNC_TYPE:
            printf("FUNC_TYPE ");
            decl_print_name_visibility(decl);
            printf("\n");
            printf("TODO\n");
            return;
        case DECL_ARRAY_VALUE:
            printf("ARRAY_VALUE ");
            decl_print_name_visibility(decl);
            printf("\n");
            printf("TODO\n");
            return;
        case DECL_LABEL:
            printf("LABEL ");
            decl_print_name_visibility(decl);
            printf("\n");
            printf("TODO\n");
            return;
        case DECL_BUILTIN:
            printf("BUILTIN TODO ");
            decl_print_name_visibility(decl);

            printf("\n");
            break;
    }
    FATAL_ERROR("TODO %d", decl->type_id);
}

void decl_print_attributes(Decl *decl, unsigned indent)
{
    printf("TODO");
}

uint64_t decl_size(Decl *decl)
{
    switch (decl->type_id)
    {
        case DECL_BUILTIN:
            return type_size(&decl->type);
        case DECL_FUNC:
        case DECL_FUNC_TYPE:
            // TODO pointer size
            return 8;
            break;
        case DECL_VAR:
            return type_size(decl->var.type);
        case DECL_ENUM_CONSTANT:
            // Go to parent
            return type_size(&decl->type);
        case DECL_ALIAS_TYPE:
            return type_size(decl->alias_decl.type);
        case DECL_STRUCT_TYPE:
            // TODO
            return 16;
        case DECL_ENUM_TYPE:
            return type_size(decl->enum_decl.type);
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
            FATAL_ERROR("Cannot have size");
    }
}

bool decl_is_type(Decl *decl)
{
    switch (decl->type_id)
    {
        case DECL_ALIAS_TYPE:
        case DECL_STRUCT_TYPE:
        case DECL_ENUM_TYPE:
        case DECL_FUNC_TYPE:
        case DECL_BUILTIN:
            return true;
        case DECL_FUNC:
        case DECL_VAR:
        case DECL_ENUM_CONSTANT:
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
            return false;
    }
}

bool decl_has_attribute(Decl *decl, AttributeType attribute_type)
{
    const AttributeInfo *info = attribute_info_from_type(attribute_type);
    if (!decl->attributes) return false;
    for (unsigned i = 0; i < decl->attributes->size; i++)
    {
        Ast *attribute = decl->attributes->entries[i];
        assert(attribute->ast_id == AST_ATTRIBUTE);
        if (token_compare_str(&attribute->attribute.name, info->name)) return true;
    }
    return false;
}

