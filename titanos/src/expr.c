//
// Created by Christoffer Lernö on 2019-02-08.
//

#include <string.h>
#include "types/type.h"
#include "expr.h"
#include "arena_allocator.h"
#include "printer.h"
#include "decl.h"
#include "diagnostics.h"

Expr *expr_copy(Expr *expr)
{
    Expr *expr_copy = malloc_arena(sizeof(Expr));
    memcpy(expr_copy, expr, sizeof(Expr));
    return expr_copy;
}

Expr *expr_new_type_expr(Type *type, SourceRange span)
{
    Expr *expr = expr_new(EXPR_TYPE, span);
    expr->type_expr.type = type;
    return expr;
}

static void expr_print_sub_list(const char *header, unsigned current_indent, Vector *list)
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
        expr_print_sub(buffer, current_indent + 1, list->entries[i]);
    }
}

Expr *expr_new(ExprTypeId type, SourceRange span)
{
    Expr *expr = malloc_arena(sizeof(Expr));
    memset(expr, 0, sizeof(Expr));
    expr->expr_id = type;
    expr->span = span;
    return expr;
}

void expr_print(Expr *expr, unsigned current_indent)
{

    indent(current_indent++);
    if (!expr)
    {
        printf("[NULL]\n");
        return;
    }
    switch (expr->expr_id)
    {
        case EXPR_CONST:
            printf("EXPR_CONST ");
            value_print(expr->const_expr.value);
            printf("\n");
            return;
        case EXPR_BINARY:
            printf("EXPR_BINARY %s\n", token_type_to_string(expr->binary_expr.operator));
            expr_print_sub("Left", current_indent, expr->binary_expr.left);
            expr_print_sub("Right", current_indent, expr->binary_expr.right);
            return;
        case EXPR_UNARY:
            printf("EXPR_UNARY %s\n", token_type_to_string(expr->unary_expr.operator));
            expr_print_sub("Expr", current_indent, expr->unary_expr.expr);
            return;
        case EXPR_POST:
            printf("EXPR_POST %s\n", token_type_to_string(expr->post_expr.operator));
            expr_print_sub("Expr", current_indent, expr->post_expr.expr);
            return;
        case EXPR_IDENTIFIER:
            printf("EXPR_IDENTIFIER [%s]\n", expr->identifier_expr.identifier);
            return;
        case EXPR_CALL:
            printf("EXPR_CALL\n");
            expr_print_sub("Function", current_indent, expr->call_expr.function);
            expr_print_sub_list("Parameters", current_indent, expr->call_expr.parameters);
            return;
        case EXPR_SUBSCRIPT:
            printf("EXPR_SUBSCRIPT\n");
            expr_print_sub("Expr", current_indent, expr->subscript_expr.expr);
            expr_print_sub("Index", current_indent, expr->subscript_expr.index);
            return;
        case EXPR_TERNARY:
            printf("EXPR_TERNARY\n");
            expr_print_sub("Expr", current_indent, expr->ternary_expr.cond);
            expr_print_sub("True", current_indent, expr->ternary_expr.then_expr);
            expr_print_sub("False", current_indent, expr->ternary_expr.else_expr);
            return;
        case EXPR_ACCESS:
            printf("EXPR_ACCESS\n");
            expr_print_sub("Parent", current_indent, expr->access_expr.parent);
            expr_print_sub("Sub Element", current_indent, expr->access_expr.sub_element);
            return;
        case EXPR_TYPE:
            printf("EXPR_TYPE %s\n", type_to_string(expr->type_expr.type));
            return;
        case EXPR_STRUCT_INIT_VALUES:
            printf("EXPR_STRUCT_INIT_VALUES\n");
            expr_print_sub_list("Values", current_indent, expr->struct_init_values_expr.values);
            return;
        case EXPR_DESIGNATED_INITIALIZER:
            printf("EXPR_DESIGNATED_INITIALIZED ");
            print_token(&expr->designated_initializer_expr.identifer);
            printf("\n");
            expr_print_sub("Value", current_indent, expr->designated_initializer_expr.expr);
            return;
        case EXPR_SIZEOF:
            printf("EXPR_SIZEOF\n");
            expr_print_sub("Expr", current_indent, expr->sizeof_expr.expr);
            return;
        case EXPR_CAST:
            printf("EXPR_CAST\n");
            expr_print_sub("Expr", current_indent, expr->cast_expr.expr);
            type_print_sub("Type", current_indent, expr->cast_expr.type);
            return;
    }
    printf("TODO %d\n", expr->expr_id);
}
void expr_print_sub(const char *header, unsigned current_indent, Expr *expr)
{
    if (!expr) return;
    indent(current_indent);
    printf("%s:\n", header);
    expr_print(expr, current_indent + 1);
}

void expr_replace(Expr *target, Expr *source)
{
    SourceRange original_span = target->span;
    *target = *source;
    target->span = original_span;
}

bool expr_is_const_false(Expr *cond)
{
    return cond->expr_id == EXPR_CONST && !value_as_bool(&cond->const_expr.value);
}
