#include <llvm-c/Core.h>
#include "constant_folding.h"
#include "ast_types.h"
#include "error.h"
#include "diagnostics.h"
#include "type_analysis.h"




static inline void replace_expr(Expr *target, Expr *source)
{
    Token original_span = target->span;
    *target = *source;
    target->span = original_span;
}

static inline bool try_conversion(Value *left, Value *right, ValueType type, const char *type_name)
{
    if (left->type == type)
    {
        return value_convert_to_type(right, type, type_name);
    }
    if (right->type == type)
    {
        return value_convert_to_type(left, type, type_name);
    }
    return false;
}

static inline bool implicit_conversion(Value *left, Value *right)
{
    if (try_conversion(left, right, VALUE_TYPE_FLOAT, "float")) return true;
    return value_convert_to_type(left, VALUE_TYPE_INT, "int") && value_convert_to_type(right, VALUE_TYPE_INT, "int");
}

typedef Value (*BinOp)(Value, Value);

static inline ExprConstState evaluate_constant_operation(Expr *expr, BinOp operation, const char *error)
{
    Value left = expr->binary_expr.left->const_expr.value;
    Value right = expr->binary_expr.right->const_expr.value;

    if (!implicit_conversion(&left, &right)) return expr->const_state = CONST_NONE;

    Value value = operation(left, right);
    if (value.type == VALUE_TYPE_ERROR)
    {
        sema_error_at(&expr->span, error,
                operation,
                value_type_name(expr->binary_expr.left->const_expr.value),
                value_type_name(expr->binary_expr.right->const_expr.value));
        return expr->const_state = CONST_NONE;
    }
    expr->const_expr.value = value;
    expr->expr_id = EXPR_CONST;
    return expr->const_state = CONST_FULL;
}


static inline ExprConstState evaluate_constant_binary_expr(Expr *expr)
{
    ExprConstState left = evaluate_constant(expr->binary_expr.left);
    if (left == CONST_UNKNOWN) return CONST_UNKNOWN;

    ExprConstState right = evaluate_constant(expr->binary_expr.right);
    if (right == CONST_UNKNOWN) return CONST_UNKNOWN;

    if (left == CONST_NONE || right == CONST_NONE) return expr->const_state = CONST_NONE;

    switch (expr->binary_expr.operator)
    {
        case TOKEN_PLUS:
            return evaluate_constant_operation(expr, &value_add, "Cant't add '%s' to '%s'");
        case TOKEN_MINUS:
            return evaluate_constant_operation(expr, &value_sub, "Can't subtract '%s' from '%s'");
        case TOKEN_STAR:
            return evaluate_constant_operation(expr, &value_mult, "Can't multiply '%s' by '%s'");;
        case TOKEN_DIV:
            return evaluate_constant_operation(expr, &value_div, "Can't divide '%s' by '%s'");
        case TOKEN_MOD:
            return evaluate_constant_operation(expr, &value_mod, "Can't get reminder of '%s' divided by '%s'");
        default:
            FATAL_ERROR("TODO");

    }
}

static inline ExprConstState evaluate_constant_ternary_expr(Expr *expr)
{
    ExprConstState decider = evaluate_constant(expr->ternary_expr.expr);

    if (decider != CONST_FULL) return decider;
    Value value = value_to_bool(expr->ternary_expr.expr->const_expr.value);
    if (value.type == VALUE_TYPE_ERROR)
    {
        return expr->const_state = CONST_NONE;
    }
    expr->ternary_expr.expr->const_expr.value = value;
    ExprConstState const_true = evaluate_constant(expr->ternary_expr.true_expr);
    ExprConstState const_false = evaluate_constant(expr->ternary_expr.true_expr);
    assert(expr->ternary_expr.expr->const_expr.value.type == VALUE_TYPE_BOOL);
    if (expr->ternary_expr.expr->const_expr.value.b)
    {
        replace_expr(expr, expr->ternary_expr.true_expr);
        return const_true;
    }
    else
    {
        replace_expr(expr, expr->ternary_expr.false_expr);
        return const_false;
    }
}

static inline ExprConstState evaluate_constant_unary_expr(Expr *expr)
{
    if (CONST_NONE == evaluate_constant(expr->unary_expr.expr))
    {
        return expr->const_state = CONST_NONE;
    }
    Expr *value = expr->unary_expr.expr;
    Value res = { .type = VALUE_TYPE_ERROR };
    switch (expr->unary_expr.operator)
    {
        case TOKEN_NOT:
            res = value_to_bool(value->const_expr.value);
            if (res.type == VALUE_TYPE_ERROR)
            {
                error_at(&value->span, "%s cannot be implictly converted to boolean", value_type_name(value->const_expr.value));
                break;
            }
            res = value_not(res);
            break;
        case TOKEN_BIT_NOT:
            res = value_not(value->const_expr.value);
            if (res.type == VALUE_TYPE_ERROR)
            {
                error_at(&value->span, "%s cannot be bit negated", value_type_name(value->const_expr.value));
            }
            break;
        case TOKEN_MINUS:
            res = value_negate(value->const_expr.value);
            if (res.type == VALUE_TYPE_ERROR)
            {
                error_at(&value->span, "%s cannot be negated", value_type_name(value->const_expr.value));
            }
            break;
        case TOKEN_PLUSPLUS:
        case TOKEN_MINUSMINUS:
        case TOKEN_STAR:
        case TOKEN_AMP:
        default:
            break;
    }
    if (res.type == VALUE_TYPE_ERROR)
    {
        return expr->const_state = CONST_NONE;
    }
    expr->const_expr.value = res;
    expr->expr_id = EXPR_CONST;
    return expr->const_state = CONST_FULL;
}

ExprConstState evaluate_constant(Expr *expr)
{
    if (expr == NULL) return CONST_FULL;
    if (expr->const_state != CONST_UNKNOWN) return expr->const_state;
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            return CONST_NONE;
        case EXPR_CONST:
            FATAL_ERROR("Should already be marked CONST_FULL");
            return CONST_FULL;
        case EXPR_UNARY:
            return evaluate_constant_unary_expr(expr);
        case EXPR_POST:
            evaluate_constant(expr->post_expr.expr);
            return CONST_NONE;
        case EXPR_TERNARY:
            return evaluate_constant_ternary_expr(expr);
        case EXPR_BINARY:
            return evaluate_constant_binary_expr(expr);
        case EXPR_SUBSCRIPT:
            break;
        case EXPR_IDENTIFIER:
            return expr->const_state = CONST_NONE;
        case EXPR_CALL:
            evaluate_constant(expr->call_expr.function);
            for (unsigned i = 0; i < expr->call_expr.parameters->size; i++)
            {
                evaluate_constant(expr->call_expr.parameters->entries[i]);
            }
            return expr->const_state = CONST_NONE;
        case EXPR_SIZEOF:break;
        case EXPR_CAST:break;
        case EXPR_ACCESS:
            evaluate_constant(expr->access_expr.parent);
            evaluate_constant(expr->access_expr.sub_element);
            return expr->const_state = CONST_NONE;
        case EXPR_STRUCT_INIT_VALUES:break;
        case EXPR_DESIGNATED_INITIALIZER:break;
    }
    FATAL_ERROR("TODO");
    return CONST_UNKNOWN;
}

