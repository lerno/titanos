//
// Created by Christoffer Lerno on 2018-12-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "ast_types.h"
#include "arena_allocator.h"
#include <stdio.h>
#include "vector.h"
#include "diagnostics.h"
#include "printer.h"
#include "module.h"



Ast *new_ast(AstType type)
{
    Ast *ast = malloc_arena(sizeof(Ast));
    ast->type = type;
    ast->span.length = 0;
    ast->const_state = CONST_UNKNOWN;
    return ast;
}

Ast *new_ast_with_span(AstType type, Token *span)
{
    Ast *ast = new_ast(type);
    ast->span = *span;
    return ast;
}

Ast *end_ast(Ast *ast, Token *end)
{
    token_expand(&ast->span, end);
    return ast;
}

static void print_sub_ast(const char *header, int current_indent, Ast *ast)
{
    if (!ast) return;
    indent(current_indent);
    printf("%s:\n", header);
    print_ast(ast, current_indent + 1);
}

static void print_sub_ast_list(const char *header, int current_indent, Vector *list)
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
        print_sub_ast(buffer, current_indent + 1, list->entries[i]);
    }
}


void print_ast(Ast *ast, int current_indent)
{
    indent(current_indent++);
    if (!ast)
    {
        printf("[NULL]\n");
        return;
    }
    switch (ast->type)
    {
        case AST_IMPORT:
            printf("IMPORT ");
            print_token(&ast->import.module_name);
            switch (ast->import.type)
            {
                case IMPORT_TYPE_FULL:
                    break;
                case IMPORT_TYPE_ALIAS:
                    printf(" alias ");
                    print_token(&ast->import.alias);
                    break;
                case IMPORT_TYPE_LOCAL:
                    printf(" local");
                    break;
            }
            printf("\n");
            return;
        case AST_FUNC_DECL:
            printf("FUNC_DECL_TYPE ");
            print_token(&ast->func_decl.name->full_name);
            printf("\n");
            print_sub_ast("Type", current_indent, ast->func_decl.r_type);
            print_sub_ast("Params", current_indent, ast->func_decl.params);
            return;
        case AST_FUNC_DEFINTION:
            printf("FUNC_DEFINTION\n");
            print_sub_ast("Decl", current_indent, ast->func_definition.func_decl);
            print_sub_ast("Body", current_indent, ast->func_definition.body);
            return;
        case AST_PARAM_LIST:
            printf("PARAM_LIST");
            if (ast->param_list.variadic)
            {
                printf(" [VARIADIC]");
            }
            printf("\n");
            print_sub_ast_list("Params", current_indent, ast->param_list.param_list);
            return;
        case AST_PARAM_DECL:
            printf("PARAM_DECL ");
            print_token(&ast->param_decl.name);
            printf("\n");
            print_sub_ast("Type", current_indent, ast->param_decl.type);
            if (ast->param_decl.default_value)
            {
                print_sub_ast("Default", current_indent, ast->param_decl.default_value);
            }
            return;
        case AST_COMPOUND_STMT:
            printf("COMPOUND_STMT\n");
            print_sub_ast_list("Lines", current_indent, ast->compound_stmt.stmts);
            return;
        case AST_IF_STMT:
            printf("IF_STMT\n");
            print_sub_ast("if", current_indent, ast->if_stmt.expr);
            print_sub_ast("body", current_indent, ast->if_stmt.if_body);
            print_sub_ast("else", current_indent, ast->if_stmt.else_body);
            return;
        case AST_WHILE_STMT:
            printf("WHILE_STMT\n");
            print_sub_ast("expr", current_indent, ast->while_stmt.expr);
            print_sub_ast("body", current_indent, ast->while_stmt.body);
            return;
        case AST_DO_STMT:
            printf("DO_STMT\n");
            print_sub_ast("expr", current_indent, ast->do_stmt.expr);
            print_sub_ast("body", current_indent, ast->do_stmt.body);
            return;
        case AST_DEFER_STMT:
            printf("DEFER_STMT\n");
            print_sub_ast("body", current_indent, ast->defer_stmt.body);
            return;
        case AST_SWITCH_STMT:
            printf("SWITCH_STMT\n");
            print_sub_ast("Expr", current_indent, ast->switch_stmt.expr);
            print_sub_ast_list("Cases", current_indent, ast->switch_stmt.case_list);
            print_sub_ast("Default", current_indent, ast->switch_stmt.default_stmt);
            return;
        case AST_CASE_STMT:
            printf("CASE_STMT\n");
            print_sub_ast("Label", current_indent, ast->case_stmt.expr);
            print_sub_ast_list("Body", current_indent, ast->case_stmt.stmts);
            return;
        case AST_DEFAULT_STMT:
            printf("DEFAULT_STMT\n");
            print_sub_ast_list("Body", current_indent, ast->default_stmt.stmts);
            return;
        case AST_BREAK_STMT:
            printf("BREAK_STMT\n");
            return;
        case AST_CONTINUE_STMT:
            printf("RETURN_STMT\n");
            return;
        case AST_RETURN_STMT:
            printf("RETURN_STMT\n");
            print_sub_ast("Expr", current_indent, ast->return_stmt.expr);
            return;
        case AST_GOTO_STMT:
            printf("GOTO STMT\n");
            print_sub_ast("Target", current_indent, ast->goto_stmt.label);
            return;
        case AST_FOR_STMT:
            printf("FOR_STMT\n");
            print_sub_ast("Init", current_indent, ast->for_stmt.init);
            print_sub_ast("Cond", current_indent, ast->for_stmt.cond);
            print_sub_ast("Incr", current_indent, ast->for_stmt.incr);
            print_sub_ast("Body", current_indent, ast->for_stmt.body);
            return;
        case AST_DECLARATION:
            printf("DECLARATION ");
            print_token(&ast->declaration.identifier);
            printf("\n");
            print_sub_ast("Type", current_indent, ast->declaration.declType);
            print_sub_ast("Init", current_indent, ast->declaration.initExpr);
            return;
        case AST_LABEL:
            printf("LABEL [");
            print_token(&ast->label_stmt.label_name);
            printf("]\n");
            return;
        case AST_FLOAT_EXPR:
            printf("FLOAT_EXPR %f\n", ast->float_expr.f);
            return;
        case AST_BOOL_EXPR:
            printf("BOOL_EXPR %s\n", ast->bool_expr.i ? "TRUE" : "FALSE");
            return;
        case AST_UINT_EXPR:
            printf("UINT_EXPR %llu\n", ast->uint_expr.u);
            return;
        case AST_INT_EXPR:
            printf("INT_EXPR %lld\n", ast->int_expr.i);
            return;
        case AST_NIL_EXPR:
            printf("NIL\n");
            return;
        case AST_BINARY_EXPR:
            printf("BINARY_EXPR %s\n", token_type_to_string(ast->binary_expr.operator));
            print_sub_ast("Left", current_indent, ast->binary_expr.left);
            print_sub_ast("Right", current_indent, ast->binary_expr.right);
            return;
        case AST_UNARY_EXPR:
            printf("UNARY_EXPR %s\n", token_type_to_string(ast->unary_expr.operator));
            print_sub_ast("Expr", current_indent, ast->unary_expr.expr);
            return;
        case AST_POST_EXPR:
            printf("POST_EXPR %s\n", token_type_to_string(ast->post_expr.operator));
            print_sub_ast("Expr", current_indent, ast->post_expr.expr);
            return;
        case AST_IDENTIFIER_EXPR:
            printf("IDENTIFIER_EXPR [");
            print_token(&ast->identifier_expr.identifier);
            printf("]\n");
            return;
        case AST_CALL_EXPR:
            printf("CALL_EXPR\n");
            print_sub_ast("Function", current_indent, ast->call_expr.function);
            print_sub_ast_list("Parameters", current_indent, ast->call_expr.parameters);
            return;
        case AST_SUBSCRIPT_EXPR:
            printf("SUBSCRIPT_EXPR\n");
            print_sub_ast("Expr", current_indent, ast->subscript_expr.expr);
            print_sub_ast("Index", current_indent, ast->subscript_expr.index);
            return;
        case AST_TERNARY_EXPR:
            printf("TERNARY_EXPR\n");
            print_sub_ast("Expr", current_indent, ast->ternary_expr.expr);
            print_sub_ast("True", current_indent, ast->ternary_expr.true_expr);
            print_sub_ast("False", current_indent, ast->ternary_expr.false_expr);
            return;
        case AST_ACCESS_EXPR:
            printf("ACCESS_EXPR\n");
            print_sub_ast("Parent", current_indent, ast->access_expr.parent);
            print_sub_ast("Sub Element", current_indent, ast->access_expr.sub_element);
            return;
        case AST_STRUCT_MEMBER:
            printf("STRUCT_MEMBER ");
            if (ast->struct_member.name.length) print_token(&ast->struct_member.name);
            printf("\n");
            switch (ast->struct_member.type)
            {
                case STRUCT_MEMBER_TYPE_NORMAL:
                    print_sub_ast("Value", current_indent, ast->struct_member.value_type);
                    break;
                case STRUCT_MEMBER_TYPE_UNION:
                    print_sub_ast_list("Union", current_indent, ast->struct_member.members);
                    break;
                case STRUCT_MEMBER_TYPE_STRUCT:
                    print_sub_ast_list("Struct", current_indent, ast->struct_member.members);
                    break;
            }
            return;
        case AST_ATTRIBUTE_LIST:
            printf("ATTRIBUTE_LIST\n");
            print_sub_ast_list("Attrs", current_indent, ast->attribute_list.list);
            return;
        case AST_ATTRIBUTE:
            printf("ATTRIBUTE ");
            print_token(&ast->attribute.name);
            print_sub_ast("Value", current_indent, ast->attribute.value);
            return;
        case AST_TYPE_EXPR:
            printf("TYPE_EXPR");
            if (ast->type_expr.flags.local) printf(" local");
            if (ast->type_expr.flags.const_ref) printf(" const");
            if (ast->type_expr.flags.alias_ref) printf(" alias");
            if (ast->type_expr.flags.volatile_ref) printf(" volatile");
            switch (ast->type_expr.type)
            {
                case TYPE_EXPR_VOID:
                    printf(" VOID\n");
                    break;
                case TYPE_EXPR_POINTER:
                    printf(" POINTER\n");
                    print_sub_ast("Type", current_indent, ast->type_expr.pointer_type_expr.type);
                    break;
                case TYPE_EXPR_ARRAY:
                    printf(" ARRAY\n");
                    print_sub_ast("Base", current_indent, ast->type_expr.array_type_expr.type);
                    if (ast->type_expr.array_type_expr.size)
                    {
                        print_sub_ast("Size", current_indent, ast->type_expr.array_type_expr.size);
                    }
                    break;
                case TYPE_EXPR_IDENTIFIER:
                    printf(" IDENTIFIER ");
                    if (ast->type_expr.identifier_type_expr.module_name.length)
                    {
                        print_token(&ast->type_expr.identifier_type_expr.module_name);
                        printf(".");
                    }
                    print_token(&ast->type_expr.identifier_type_expr.name);
                    if (!ast->type_expr.identifier_type_expr.resolved_type)
                    {
                        printf(" [UNRESOLVED]");
                    }
                    printf("\n");
                    print_sub_ast("Type", current_indent, ast->type_expr.identifier_type_expr.resolved_type);
                    break;
                default:
                    printf("Unknown!\n");
            }
            return;
        case AST_STRUCT_INIT_VALUES_EXPR:
            printf("STRUCT_INIT_VALUES_EXPR\n");
            print_sub_ast_list("Values", current_indent, ast->struct_init_values_expr.values);
            return;
        case AST_STRING_EXPR:
            printf("STRING_EXPR\n");
            indent(current_indent);
            printf("Text: ");
            print_token(&ast->string_expr.string);
            printf("\n");
            print_sub_ast("Next", current_indent, ast->string_expr.next_string);
            return;
        case AST_DESIGNATED_INITIALIZED_EXPR:
            printf("DESIGNATED_INITIALIZED_EXPR ");
            print_token(&ast->designated_initializer_expr.identifer);
            printf("\n");
            print_sub_ast("Value", current_indent, ast->designated_initializer_expr.expr);
            return;
        case AST_TYPE_DEFINITION:
            printf("TYPE_DEFINITION ");
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
            return;
        case AST_VAR_DEFINITION:
            printf("VAR_DEFINITION ");
            print_token(&ast->var_definition.name);
            if (ast->var_definition.is_public) printf(" public");
            if (ast->var_definition.is_exported) printf(" exported");
            printf("\n");
            print_sub_ast("Type", current_indent, ast->var_definition.type);
            print_sub_ast("Value", current_indent, ast->var_definition.value);
            print_sub_ast("Attributes", current_indent, ast->var_definition.attributes);
            return;
        case AST_ENUM_ENTRY:
            printf("ENUM_ENTRY ");
            print_token(&ast->enum_entry.name);
            printf("\n");
            print_sub_ast("Value", current_indent, ast->enum_entry.value);
            return;
        case AST_INCREMENTAL_ARRAY:
            printf("INCREMENTAL_ARRAY ");
            print_token(&ast->incremental_array.name);
            print_sub_ast("Value", current_indent, ast->incremental_array.value);
            return;
        case AST_SIZEOF_EXPR:
            printf("SIZEOF_EXPR\n");
            print_sub_ast("Expr", current_indent, ast->sizeof_expr.expr);
            return;
        case AST_CAST_EXPR:
            printf("CAST_EXPR\n");
            print_sub_ast("Expr", current_indent, ast->cast_expr.expr);
            print_sub_ast("Type", current_indent, ast->cast_expr.type);
            return;
    }
    printf("TODO\n");
}


Ast *new_type_expr(TypeExprType type_expr, Token *span)
{
    Ast *ast = new_ast_with_span(AST_TYPE_EXPR, span);
    ast->type_expr.type = type_expr;
    ast->type_expr.flags.alias_ref = false;
    ast->type_expr.flags.const_ref = false;
    ast->type_expr.flags.local = false;
    ast->type_expr.flags.resolved = false;
    ast->type_expr.flags.volatile_ref = false;
    return ast;
}

Ast *new_type_definition(DefinitionType type, Token *name, bool public, Token *initial_token)
{
    Ast *ast = new_ast_with_span(AST_TYPE_DEFINITION, initial_token);
    ast->definition.definition_type = type;
    ast->definition.name = *name;
    ast->definition.is_public = public;
	ast->definition.is_exported = false;
	ast->definition.is_incremental = false;
	ast->definition.is_struct = false;
	ast->definition.is_used_public = false;
	ast->definition.module = NULL;
	ast->definition.attributes = NULL;
	return ast;
}
