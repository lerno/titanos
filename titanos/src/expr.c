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


BinOp bin_op[256] = {
        [TOKEN_EQ] = BINOP_ASSIGN,
        [TOKEN_STAR] = BINOP_MULT,
        [TOKEN_MULT_ASSIGN] = BINOP_MULT_ASSIGN,
        [TOKEN_PLUS] = BINOP_ADD,
        [TOKEN_PLUS_ASSIGN] = BINOP_ADD_ASSIGN,
        [TOKEN_MINUS] = BINOP_SUB,
        [TOKEN_MINUS_ASSIGN] = BINOP_SUB_ASSIGN,
        [TOKEN_DIV] = BINOP_DIV,
        [TOKEN_DIV_ASSIGN] = BINOP_DIV_ASSIGN,
        [TOKEN_MOD] = BINOP_MOD,
        [TOKEN_MOD_ASSIGN] = BINOP_MOD_ASSIGN,
        [TOKEN_NOT_EQUAL] = BINOP_NE,
        [TOKEN_AND] = BINOP_AND,
        [TOKEN_AND_ASSIGN] = BINOP_AND_ASSIGN,
        [TOKEN_OR] = BINOP_OR,
        [TOKEN_OR_ASSIGN] = BINOP_OR_ASSIGN,
        [TOKEN_AMP] = BINOP_BIT_AND,
        [TOKEN_BIT_AND_ASSIGN] = BINOP_BIT_AND_ASSIGN,
        [TOKEN_BIT_OR] = BINOP_BIT_OR,
        [TOKEN_BIT_OR_ASSIGN] = BINOP_BIT_OR_ASSIGN,
        [TOKEN_BIT_XOR] = BINOP_BIT_XOR,
        [TOKEN_BIT_XOR_ASSIGN] = BINOP_BIT_XOR_ASSIGN,
        [TOKEN_EQEQ] = BINOP_EQ,
        [TOKEN_GREATER] = BINOP_GT,
        [TOKEN_GREATER_EQ] = BINOP_GE,
        [TOKEN_LESS] = BINOP_LT,
        [TOKEN_LESS_EQ] = BINOP_LE,
        [TOKEN_RIGHT_SHIFT] = BINOP_SHR,
        [TOKEN_RIGHT_SHIFT_ASSIGN] = BINOP_SHR_ASSIGN,
        [TOKEN_LEFT_SHIFT] = BINOP_SHL,
        [TOKEN_LEFT_SHIFT_ASSIGN] = BINOP_SHL_ASSIGN,
        [TOKEN_ELVIS] = BINOP_ELVIS
};

UnaryOp unary_op[256] = {
        [TOKEN_STAR] = UNARYOP_DEREF,
        [TOKEN_AMP] = UNARYOP_ADDR,
        [TOKEN_BIT_NOT] = UNARYOP_BITNEG,
        [TOKEN_NOT] = UNARYOP_NOT,
        [TOKEN_MINUS] = UNARYOP_NEG,
        [TOKEN_PLUSPLUS] = UNARYOP_INC,
        [TOKEN_MINUSMINUS] = UNARYOP_DEC,
};

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
            printf("EXPR_BINARY %s\n", token_type_to_string(binop_to_token(expr->binary_expr.operator)));
            expr_print_sub("Left", current_indent, expr->binary_expr.left);
            expr_print_sub("Right", current_indent, expr->binary_expr.right);
            return;
        case EXPR_UNARY:
            printf("EXPR_UNARY %s\n", token_type_to_string(unaryop_to_token(expr->unary_expr.operator)));
            expr_print_sub("Expr", current_indent, expr->unary_expr.expr);
            return;
        case EXPR_POST:
            printf("EXPR_POST %s\n", token_type_to_string(unaryop_to_token(expr->post_expr.operator)));
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
            printf("EXPR_DESIGNATED_INITIALIZED %s\n", expr->designated_initializer_expr.identifer);
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

BinOp binop_from_token(TokenType type)
{
    return bin_op[type];
}

TokenType binop_to_token(BinOp type)
{
    for (unsigned i = 0; i < 256; i++)
    {
        if (bin_op[i] == type) return (TokenType)i;
    }
    return TOKEN_ERROR;
}


UnaryOp unaryop_from_token(TokenType type)
{
    return unary_op[type];
}

TokenType unaryop_to_token(UnaryOp type)
{
    for (unsigned i = 0; i < 256; i++)
    {
        if (unary_op[i] == type) return (TokenType)i;
    }
    return TOKEN_ERROR;
}
