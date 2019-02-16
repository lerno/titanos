#include <llvm-c/Core.h>
#include "constant_folding.h"
#include "ast_types.h"
#include "error.h"
#include "diagnostics.h"
#include "type_analysis.h"
#include "expression_analysis.h"


static inline ExprConstState replace_with_constant(Expr *target, Value *source)
{
    target->const_expr.value = *source;
    target->const_state = CONST_FULL;
    target->expr_id = EXPR_CONST;
    target->type = value_find_type(source);
    return CONST_FULL;
}

static inline void replace_expr(Expr *target, Expr *source)
{
    Token original_span = target->span;
    *target = *source;
    target->span = original_span;
}


static inline bool implicit_conversion(Expr *left, Expr *right)
{
    return value_convert_to_type(&left->const_expr.value, &right->const_expr.value);
}

typedef Value (*BinOp)(Value, Value);

static inline ExprConstState evaluate_constant_operation(Expr *expr, BinOp operation, const char *error, bool req_int)
{
    Expr *left = expr->binary_expr.left;
    Expr *right = expr->binary_expr.right;

    if (req_int && (right->const_expr.value.type != VALUE_TYPE_INT || left->const_expr.value.type == VALUE_TYPE_INT))
    {
        sema_error_at(&expr->span, error, value_type_name(&left->const_expr.value), value_type_name(&right->const_expr.value));
        expr->type->type_id = TYPE_INVALID;
        return expr->const_state = CONST_NONE;
    }
    if (!implicit_conversion(left, right))
    {
        sema_error_at(&expr->span, "Can't implicitly convert between '%s' and '%s'", value_type_name(&left->const_expr.value), value_type_name(&right->const_expr.value));
        expr->type->type_id = TYPE_INVALID;
        return expr->const_state = CONST_NONE;
    }
    Value value = operation(left->const_expr.value, right->const_expr.value);
    if (value.type == VALUE_TYPE_ERROR)
    {
        sema_error_at(&expr->span, error, value_type_name(&left->const_expr.value), value_type_name(&right->const_expr.value));
        expr->type->type_id = TYPE_INVALID;
        return expr->const_state = CONST_NONE;
    }
    return replace_with_constant(expr, &value);
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
            return evaluate_constant_operation(expr, &value_add, "Cant't add '%s' to '%s'", false);
        case TOKEN_MINUS:
            return evaluate_constant_operation(expr, &value_sub, "Can't subtract '%s' from '%s'", false);
        case TOKEN_STAR:
            return evaluate_constant_operation(expr, &value_mult, "Can't multiply '%s' by '%s'", false);;
        case TOKEN_DIV:
            return evaluate_constant_operation(expr, &value_div, "Can't divide '%s' by '%s'", false);
        case TOKEN_MOD:
            return evaluate_constant_operation(expr, &value_mod, "Can't get reminder of '%s' divided by '%s'", false);
        case TOKEN_LEFT_SHIFT:
            TODO;
        case TOKEN_RIGHT_SHIFT:
            TODO;
        case TOKEN_BIT_XOR:
            TODO;
        case TOKEN_BIT_OR:
            TODO;
        case TOKEN_AMP:
            TODO;
        case TOKEN_EQEQ:
            TODO;
        case TOKEN_GREATER_EQ:
            TODO;
        case TOKEN_LESS_EQ:
            TODO;
        case TOKEN_LESS:
            TODO;
        case TOKEN_GREATER:
            TODO;
        case TOKEN_NOT_EQUAL:
            TODO;
        case TOKEN_ELVIS:
            TODO;
        default:
            FATAL_ERROR("TODO");

    }
}

static inline ExprConstState evaluate_constant_ternary_expr(Expr *expr)
{
    ExprConstState decider = evaluate_constant(expr->ternary_expr.cond);

    if (decider != CONST_FULL) return decider;
    Value value = value_to_bool(expr->ternary_expr.cond->const_expr.value);
    if (value.type == VALUE_TYPE_ERROR)
    {
        return expr->const_state = CONST_NONE;
    }
    expr->ternary_expr.cond->const_expr.value = value;
    ExprConstState const_true = evaluate_constant(expr->ternary_expr.then_expr);
    ExprConstState const_false = evaluate_constant(expr->ternary_expr.then_expr);
    assert(expr->ternary_expr.cond->const_expr.value.type == VALUE_TYPE_BOOL);
    if (expr->ternary_expr.cond->const_expr.value.b)
    {
        replace_expr(expr, expr->ternary_expr.then_expr);
        return const_true;
    }
    else
    {
        replace_expr(expr, expr->ternary_expr.else_expr);
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
                sema_error_at(&value->span, "%s cannot be implictly converted to boolean", value_type_name(&value->const_expr.value));
                break;
            }
            res = value_not(res);
            break;
        case TOKEN_BIT_NOT:
            res = value_not(value->const_expr.value);
            if (res.type == VALUE_TYPE_ERROR)
            {
                sema_error_at(&value->span, "%s cannot be bit negated", value_type_name(&value->const_expr.value));
            }
            break;
        case TOKEN_MINUS:
            res = value_negate(value->const_expr.value);
            if (res.type == VALUE_TYPE_ERROR)
            {
                sema_error_at(&value->span, "%s cannot be negated", value_type_name(&value->const_expr.value));
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
    return replace_with_constant(expr, &res);
}

ExprConstState evaluate_constant_identifer(Expr *identifier)
{
    LOG_FUNC
    if (!analyse_expr(identifier, RHS)) return CONST_UNKNOWN;
    Decl *decl = identifier->identifier_expr.resolved;
    if (!decl) return CONST_UNKNOWN;
    switch (decl->type_id)
    {
        case DECL_BUILTIN:
            identifier->expr_id = EXPR_TYPE;
            identifier->type_expr.type = &decl->type;
            return CONST_FULL;
        case DECL_FUNC:
            // TODO revisit
            return CONST_NONE;
        case DECL_VAR:
            evaluate_constant(decl->var.init_expr);
            if (decl->var.init_expr->const_state == CONST_FULL)
            {
                replace_expr(identifier, decl->var.init_expr);
            }
            return decl->var.init_expr->const_state;
        case DECL_ENUM_CONSTANT:
            // TODO check that this is evaluated
            assert(evaluate_constant(decl->enum_constant.init_value) == CONST_FULL);
            replace_expr(identifier, decl->enum_constant.init_value);
            return CONST_FULL;
        case DECL_ALIAS_TYPE:
            identifier->expr_id = EXPR_TYPE;
            identifier->type_expr.type = decl->alias_decl.type;
            return CONST_FULL;
        case DECL_STRUCT_TYPE:
        case DECL_ENUM_TYPE:
        case DECL_FUNC_TYPE:
            identifier->expr_id = EXPR_TYPE;
            identifier->type_expr.type = &decl->type;
            return CONST_FULL;
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
            return CONST_NONE;
    }
    UNREACHABLE
}

// Assumes that all checks are already done.
static ExprConstState evaluate_constant_without_checks(Expr *expr)
{
    LOG_FUNC
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
            return evaluate_constant_identifer(expr);
        case EXPR_CALL:
            evaluate_constant(expr->call_expr.function);
            for (unsigned i = 0; i < expr->call_expr.parameters->size; i++)
            {
                evaluate_constant(expr->call_expr.parameters->entries[i]);
            }
            return expr->const_state = CONST_NONE;
        case EXPR_SIZEOF:
            if (!analyse_expr(expr->sizeof_expr.expr, RHS)) return CONST_UNKNOWN;
            if (!resolve_type(&expr->sizeof_expr.expr->type, false)) return CONST_UNKNOWN;
            expr->expr_id = EXPR_CONST;
            bigint_init_unsigned(&expr->const_expr.value.big_int, type_size(expr->sizeof_expr.expr->type));
            break;
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

ExprConstState evaluate_constant(Expr *expr)
{
    LOG_FUNC
    if (expr == NULL) return CONST_FULL;
    if (expr->is_evaluating)
    {
        sema_error_at(&expr->span, "Recursive expression found");
        return expr->const_state = CONST_NONE;
    }
    if (expr->const_state != CONST_UNKNOWN) return expr->const_state;
    expr->is_evaluating = true;
    ExprConstState state = evaluate_constant_without_checks(expr);
    expr->is_evaluating = false;
    return state;
}
