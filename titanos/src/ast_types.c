//
// Created by Christoffer Lerno on 2018-12-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "ast_types.h"
#include "arena_allocator.h"
#include <stdio.h>
#include "types/type.h"
#include "vector.h"
#include "diagnostics.h"
#include "printer.h"
#include "module.h"
#include "expr.h"


Ast *new_ast(AstType type)
{
    Ast *ast = malloc_arena(sizeof(Ast));
    ast->exit = EXIT_NONE;
    ast->ast_id = type;
    ast->span.length = 0;
    return ast;
}

Ast *new_ast_with_span(AstType type, SourceRange span)
{
    Ast *ast = new_ast(type);
    ast->span = span;
    ast->exit = EXIT_NONE;
    return ast;
}


CondValue ast_cond_value(Ast *cond)
{
    assert(cond->ast_id == AST_COND_STMT);
    switch (cond->cond_stmt.cond_type)
    {
        case COND_EXPR:
            if (cond->cond_stmt.expr->expr_id != EXPR_CONST) return COND_VARIABLE;
            assert(cond->cond_stmt.expr->const_expr.value.type == VALUE_TYPE_BOOL);
            return cond->cond_stmt.expr->const_expr.value.b ? COND_TRUE : COND_FALSE;
        case COND_DECL:
            TODO;
            return COND_VARIABLE;
    }
    UNREACHABLE
}

void print_sub_ast(const char *header, unsigned int current_indent, Ast *ast)
{
    if (!ast) return;
    indent(current_indent);
    printf("%s:\n", header);
    print_ast(ast, current_indent + 1);
}

static void print_sub_ast_list(const char *header, unsigned current_indent, Vector *list)
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
    switch (ast->ast_id)
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
            print_sub_ast("if", current_indent, ast->if_stmt.cond);
            print_sub_ast("body", current_indent, ast->if_stmt.then_body);
            print_sub_ast("else", current_indent, ast->if_stmt.else_body);
            return;
        case AST_WHILE_STMT:
            printf("WHILE_STMT\n");
            print_sub_ast("expr", current_indent, ast->while_stmt.cond);
            print_sub_ast("body", current_indent, ast->while_stmt.body);
            return;
        case AST_DO_STMT:
            printf("DO_STMT\n");
            expr_print_sub("expr", current_indent, ast->do_stmt.expr);
            print_sub_ast("body", current_indent, ast->do_stmt.body);
            return;
        case AST_DEFER_STMT:
            printf("DEFER_STMT\n");
            print_sub_ast("body", current_indent, ast->defer_stmt.body);
            return;
        case AST_SWITCH_STMT:
            printf("SWITCH_STMT\n");
            print_sub_ast("Expr", current_indent, ast->switch_stmt.cond);
            print_sub_ast_list("Cases", current_indent, ast->switch_stmt.case_list);
            print_sub_ast("Default", current_indent, ast->switch_stmt.default_stmt);
            return;
        case AST_CASE_STMT:
            printf("CASE_STMT\n");
            expr_print_sub("Label", current_indent, ast->case_stmt.expr);
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
            expr_print_sub("Expr", current_indent, ast->return_stmt.expr);
            return;
        case AST_GOTO_STMT:
            printf("GOTO STMT ");
            if (ast->goto_stmt.type == GOTO_NOT_ANALYSED)
            {
                printf("%s", ast->goto_stmt.label_name);
            }
            else
            {
                printf("%s", ast->goto_stmt.label->name);
            }
            printf("\n");
            return;
        case AST_FOR_STMT:
            printf("FOR_STMT\n");
            print_sub_ast("Init", current_indent, ast->for_stmt.init);
            expr_print_sub("Cond", current_indent, ast->for_stmt.cond);
            expr_print_sub("Incr", current_indent, ast->for_stmt.incr);
            print_sub_ast("Body", current_indent, ast->for_stmt.body);
            return;
        case AST_LABEL:
            printf("LABEL [%s]", ast->label_stmt.label_name);
            printf("]");
            if (ast->label_stmt.is_used) printf(" used");
            printf("\n");
            return;
        case AST_COND_STMT:
            printf("COND_STMT ");
            printf("\n");
            switch (ast->cond_stmt.cond_type)
            {
                case COND_EXPR:
                    expr_print_sub("Expr", current_indent, ast->cond_stmt.expr);
                    return;
                case COND_DECL:
                    decl_print_sub("Decl", current_indent, ast->cond_stmt.decl);
                    return;
            }
            UNREACHABLE
        case AST_EXPR_STMT:
            printf("EXPR_STMT ");
            printf("\n");
            expr_print_sub("Expr", current_indent, ast->expr_stmt.expr);
            return;
        case AST_ATTRIBUTE:
            printf("ATTRIBUTE %s", ast->attribute.name);
            expr_print_sub("Value", current_indent, ast->attribute.value);
            return;
        case AST_DECLARE_STMT:
            printf("DECLARE_STMT\n");
            decl_print_sub("Decl", current_indent, ast->declare_stmt.decl);
            return;
        case AST_DEFER_RELASE:
            printf("DEFER_RELEASE\n");
            print_sub_ast("Inner", current_indent, ast->defer_release_stmt.inner);
            print_sub_ast("DeferStart", current_indent, ast->defer_release_stmt.list.defer_start);
            print_sub_ast("DeferEnd", current_indent, ast->defer_release_stmt.list.defer_end);
            return;
    }
    printf("TODO %d\n", ast->ast_id);
}




Ast *ast_compound_stmt_last(Ast *compound_stmt)
{
    assert(compound_stmt->ast_id == AST_COMPOUND_STMT);
    for (unsigned i = compound_stmt->compound_stmt.stmts->size; i > 0; i--)
    {
        Ast *last = compound_stmt->compound_stmt.stmts->entries[i];
        if (last->ast_id == AST_COMPOUND_STMT)
        {
            last = ast_compound_stmt_last(last);
        }
        if (last) return last;
    }
    return NULL;
}
