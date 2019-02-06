//
// Created by Christoffer Lerno on 2018-12-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "ast_types.h"
#include "arena_allocator.h"
#include <stdio.h>
#include <types/type.h>
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

void print_sub_ast(const char *header, unsigned int current_indent, Ast *ast)
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


void print_ast(Ast *ast, unsigned current_indent)
{
    indent(current_indent++);
    if (!ast)
    {
        printf("[NULL]\n");
        return;
    }
    switch (ast->type)
    {
        case AST_COMPOUND_STMT:
            printf("COMPOUND_STMT\n");
            print_sub_ast_list("Lines", current_indent, ast->compound_stmt.stmts);
            return;
        case AST_ASM_STMT:
            printf("ASM_STMT\n");
            printf("TODO");
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
        case AST_LABEL:
            printf("LABEL [");
            print_token(&ast->label_stmt.label_name);
            printf("]");
            if (ast->label_stmt.is_used) printf(" used");
            printf("\n");
            return;
        case AST_CONST_EXPR:
            printf("CONST_EXPR ");
            value_print(ast->const_expr.value);
            printf("\n");
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
                    print_sub_ast("Size", current_indent, ast->type_expr.array_type_expr.size);
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
        case AST_DESIGNATED_INITIALIZED_EXPR:
            printf("DESIGNATED_INITIALIZED_EXPR ");
            print_token(&ast->designated_initializer_expr.identifer);
            printf("\n");
            print_sub_ast("Value", current_indent, ast->designated_initializer_expr.expr);
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
        case AST_DECLARE_STMT:
            printf("DECLARE_STMT\n");
            decl_print_sub("Decl", current_indent, ast->declare_stmt.decl);
        case AST_DEFER_RELASE:
            printf("DEFER_RELEASE\n");
            print_sub_ast("Inner", current_indent, ast->defer_release_stmt.inner);
            print_sub_ast("DeferStart", current_indent, ast->defer_release_stmt.list.defer_start);
            print_sub_ast("DeferEnd", current_indent, ast->defer_release_stmt.list.defer_end);
            return;
    }
    printf("TODO %d\n", ast->type);
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


Ast *ast_compound_stmt_last(Ast *compound_stmt)
{
    assert(compound_stmt->type == AST_COMPOUND_STMT);
    for (unsigned i = compound_stmt->compound_stmt.stmts->size; i > 0; i--)
    {
        Ast *last = compound_stmt->compound_stmt.stmts->entries[i];
        if (last->type == AST_COMPOUND_STMT)
        {
            last = ast_compound_stmt_last(last);
        }
        if (last) return last;
    }
    return NULL;
}
