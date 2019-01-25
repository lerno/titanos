#include <llvm-c/Core.h>
#include "constant_folding.h"
#include "ast_types.h"
#include "error.h"
#include "diagnostics.h"


static inline AstConstState evaluate_constant_type_expr(Ast *type_expr)
{
    assert(type_expr->type == AST_TYPE_EXPR);

    // Improve: with macros, identifiers will be evaluated.
    switch (type_expr->type_expr.type)
    {
        case TYPE_EXPR_IDENTIFIER:
            type_expr->const_state = CONST_FULL;
            break;
        case TYPE_EXPR_ARRAY:
            type_expr->const_state = evaluate_constant(type_expr->type_expr.array_type_expr.size) ? CONST_FULL : CONST_NONE;
            break;
        case TYPE_EXPR_POINTER:
            type_expr->const_state = evaluate_constant_type_expr(type_expr->type_expr.pointer_type_expr.type) ? CONST_FULL : CONST_NONE;
        case TYPE_EXPR_VOID:
            type_expr->const_state = CONST_FULL;
        default:
            FATAL_ERROR("Not reachable");
    }
    return type_expr->const_state;
}


static inline void replace_ast(Ast *target, Ast *source)
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
    if (try_conversion(left, right, TYPE_FLOAT, "float")) return true;
    if (try_conversion(left, right, TYPE_INT, "int")) return true;
    if (try_conversion(left, right, TYPE_UINT, "int")) return true;
    if (!value_convert_to_type(left, TYPE_INT, "int")) return false;
    return value_convert_to_type(right, TYPE_INT, "int");
}

static inline AstConstState evaluate_constant_plus(Ast *ast)
{
    Value left = ast->binary_expr.left->const_expr.value;
    Value right = ast->binary_expr.right->const_expr.value;

    if (!implicit_conversion(&left, &right)) return ast->const_state = CONST_NONE;

    Value value = value_mult(left, right);
    if (value.type == TYPE_ERROR)
    {
        sema_error_at(&ast->span, "Can't multiply '%s' and '%s'",
                value_type_name(ast->binary_expr.left->const_expr.value),
                value_type_name(ast->binary_expr.right->const_expr.value));
        return ast->const_state = CONST_NONE;
    }

    ast->const_expr.value = value;
    ast->type = AST_CONST_EXPR;
    return ast->const_state = CONST_FULL;
}

static inline AstConstState evaluate_constant_minus(Ast *ast)
{
    Value left = ast->binary_expr.left->const_expr.value;
    Value right = ast->binary_expr.right->const_expr.value;

    if (!implicit_conversion(&left, &right)) return ast->const_state = CONST_NONE;

    Value value = value_sub(left, right);
    if (value.type == TYPE_ERROR)
    {
        sema_error_at(&ast->span, "Can't subtract '%s' from '%s'",
                      value_type_name(ast->binary_expr.left->const_expr.value),
                      value_type_name(ast->binary_expr.right->const_expr.value));
        return ast->const_state = CONST_NONE;
    }

    ast->const_expr.value = value;
    ast->type = AST_CONST_EXPR;
    return ast->const_state = CONST_FULL;
}

static inline AstConstState evaluate_constant_mult(Ast *ast)
{
    Value *left = ast->binary_expr.left->const_expr.value;
    Value *right = ast->binary_expr.right->const_expr.value;

    if (!implicit_conversion(&left, &right)) return ast->const_state = CONST_NONE;

    Value *result = value_mult(left, right);
    if (result)
    {
        ast->const_expr.value = result;
        ast->type = AST_CONST_EXPR;
        return ast->const_state = CONST_FULL;
    }
    else
    {
        return ast->const_state = CONST_NONE;
    }
}

static inline AstConstState evaluate_constant_binary_expr(Ast *ast)
{
    AstConstState left = evaluate_constant(ast->binary_expr.left);
    if (left == CONST_UNKNOWN) return CONST_UNKNOWN;

    AstConstState right = evaluate_constant(ast->binary_expr.right);
    if (right == CONST_UNKNOWN) return CONST_UNKNOWN;

    if (left == CONST_NONE || right == CONST_NONE) return ast->const_state = CONST_NONE;

    switch (ast->binary_expr.operator)
    {
        case TOKEN_PLUS:
            return evaluate_constant_plus(ast);
        case TOKEN_MINUS:
            return evaluate_constant_minus(ast);
        case TOKEN_STAR:
            return evaluate_constant_mult(ast);
        default:
            FATAL_ERROR("TODO");

    }
}

static inline AstConstState evaluate_constant_ternary_expr(Ast *ast)
{
    AstConstState decider = evaluate_constant(ast->ternary_expr.expr);

    if (decider != CONST_FULL) return decider;
    if (!value_to_bool(ast->ternary_expr.expr->const_expr.value))
    {
        return ast->const_state = CONST_NONE;
    }
    AstConstState const_true = evaluate_constant(ast->ternary_expr.true_expr);
    AstConstState const_false = evaluate_constant(ast->ternary_expr.true_expr);

    // Both sides must be constant
    if (const_true != CONST_FULL || const_false != CONST_FULL)
    {
        return ast->const_state = CONST_NONE;
    }

    if (value_bool_value(ast->ternary_expr.expr->const_expr.value))
    {
        replace_ast(ast, ast->ternary_expr.true_expr);
    }
    else
    {
        replace_ast(ast, ast->ternary_expr.false_expr);
    }
    return CONST_FULL;
}

static inline AstConstState evaluate_constant_unary_expr(Ast *ast)
{
    if (CONST_NONE == evaluate_constant(ast->unary_expr.expr))
    {
        return ast->const_state = CONST_NONE;
    }
    Ast *value = ast->unary_expr.expr;
    Value *res = NULL;
    switch (ast->unary_expr.operator)
    {
        case TOKEN_NOT:
            res = value_to_bool(value->const_expr.value);
            if (!res)
            {
                error_at(&value->span, "%s cannot be implictly converted to boolean", value_type_name(value->const_expr.value));
                break;
            }
            res = value_not(res);
            break;
        case TOKEN_BIT_NOT:
            res = value_not(value->const_expr.value);
            if (!res)
            {
                error_at(&value->span, "%s cannot be bit negated", value_type_name(value->const_expr.value));
            }
            break;
        case TOKEN_MINUS:
            res = value_negate(value->const_expr.value);
            if (!res)
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
    if (!res)
    {
        return ast->const_state = CONST_NONE;
    }
    ast->const_expr.value = res;
    ast->type = AST_CONST_EXPR;
    return ast->const_state = CONST_FULL;
}

AstConstState evaluate_constant(Ast *ast)
{
    if (ast == NULL) return CONST_FULL;
    if (ast->const_state != CONST_UNKNOWN) return ast->const_state;
    switch (ast->type)
    {
        case AST_TYPE_EXPR:
            return evaluate_constant_type_expr(ast);
        case AST_CONST_EXPR:
            FATAL_ERROR("Should already be marked CONST_FULL");
            return CONST_FULL;
        case AST_UNARY_EXPR:
            return evaluate_constant_unary_expr(ast);
        case AST_POST_EXPR:
            evaluate_constant(ast->post_expr.expr);
            return CONST_NONE;
        case AST_TERNARY_EXPR:
            return evaluate_constant_ternary_expr(ast);
        case AST_BINARY_EXPR:
            return evaluate_constant_binary_expr(ast);
        case AST_IDENTIFIER_EXPR:
        case AST_CALL_EXPR:break;
        case AST_SIZEOF_EXPR:break;
        case AST_CAST_EXPR:break;
        case AST_SUBSCRIPT_EXPR:break;
        case AST_ACCESS_EXPR:break;
        case AST_STRUCT_INIT_VALUES_EXPR:break;
        case AST_DESIGNATED_INITIALIZED_EXPR:break;
        default:
            FATAL_ERROR("Unexpected type %d for folding", ast->type);
            return CONST_NONE;
    }
    FATAL_ERROR("TODO");
    return CONST_UNKNOWN;
}

