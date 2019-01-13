#include "constant_folding.h"
#include "ast_types.h"
#include "error.h"


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
            FATAL_ERRORF("Not reachable");
    }
    return type_expr->const_state;
}

static inline bool convert_to_bool(Ast *const_ast)
{
    switch (const_ast->type)
    {
        case AST_UINT_EXPR:
            const_ast->bool_expr.i = const_ast->uint_expr.u  != 0;
            break;
        case AST_INT_EXPR:
            const_ast->bool_expr.i = const_ast->int_expr.i  != 0;
            break;
        case AST_NIL_EXPR:
            const_ast->bool_expr.i = false;
            break;
        case AST_FLOAT_EXPR:
            const_ast->float_expr.f = const_ast->float_expr.f != 0.0;
            break;
        default:
            return false;
    }
    const_ast->type = AST_BOOL_EXPR;
    return true;
}

static inline void replace_ast(Ast *target, Ast *source)
{
    Token original_span = target->span;
    *target = *source;
    target->span = original_span;
}

static inline void implicit_conversion(Ast *left, Ast *right)
{
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
        case TOKEN_STAR:
//            return evaluate_constant_mult(ast);
        default:
            FATAL_ERRORF("TODO");

    }
}

static inline AstConstState evaluate_constant_ternary_expr(Ast *ast)
{
    AstConstState decider = evaluate_constant(ast->ternary_expr.expr);

    if (decider != CONST_FULL) return decider;
    if (!convert_to_bool(ast->ternary_expr.expr))
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

    if (ast->ternary_expr.expr->bool_expr.i)
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
    Ast *expression = ast->unary_expr.expr;
    switch (ast->unary_expr.operator)
    {

        case TOKEN_NOT:
            switch (expression->type)
            {
                case AST_INT_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = !expression->int_expr.i;
                    return ast->const_state = CONST_FULL;
                case AST_UINT_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = !expression->uint_expr.u;
                    return ast->const_state = CONST_FULL;
                case AST_FLOAT_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = !expression->float_expr.f;
                    return ast->const_state = CONST_FULL;
                case AST_BOOL_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = !expression->bool_expr.i;
                    return ast->const_state = CONST_FULL;
                case AST_NIL_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = true;
                    return ast->const_state = CONST_FULL;
                default:
                    return ast->const_state = CONST_NONE;
            }
        case TOKEN_BIT_NOT:
            switch (expression->type)
            {
                case AST_UINT_EXPR:
                    ast->type = AST_UINT_EXPR;
                    ast->uint_expr.u = ~expression->uint_expr.u;
                    return ast->const_state = CONST_FULL;
                case AST_INT_EXPR:
                    ast->type = AST_INT_EXPR;
                    ast->int_expr.i = ~expression->int_expr.i;
                    return ast->const_state = CONST_FULL;
                case AST_FLOAT_EXPR:
                    return ast->const_state = CONST_NONE;
                case AST_BOOL_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = !expression->bool_expr.i;
                    return ast->const_state = CONST_NONE;
                case AST_NIL_EXPR:
                    ast->type = AST_BOOL_EXPR;
                    ast->bool_expr.i = true;
                    return ast->const_state = CONST_FULL;
                default:
                    return ast->const_state = CONST_NONE;
            }
        case TOKEN_MINUS:
        {
            switch (expression->type)
            {
                case AST_INT_EXPR:
                    ast->type = AST_INT_EXPR;
                    ast->int_expr.i = -expression->int_expr.i;
                    return ast->const_state = CONST_FULL;
                case AST_UINT_EXPR:
                    ast->type = AST_INT_EXPR;
                    // TODO fix overflow
                    ast->int_expr.i = -((int64_t)expression->uint_expr.u);
                    return ast->const_state = CONST_FULL;
                case AST_FLOAT_EXPR:
                    ast->type = AST_FLOAT_EXPR;
                    ast->float_expr.f = -expression->float_expr.f;
                    return ast->const_state = CONST_FULL;
                case AST_BOOL_EXPR:
                    ast->type = AST_INT_EXPR;
                    ast->int_expr.i = expression->bool_expr.i ? -1 : 0;
                    return ast->const_state = CONST_FULL;
                case AST_NIL_EXPR:
                    ast->type = AST_INT_EXPR;
                    ast->int_expr.i = 0;
                    return ast->const_state = CONST_FULL;
                default:
                    return ast->const_state = CONST_NONE;
            }
        }
        case TOKEN_PLUSPLUS:
        case TOKEN_MINUSMINUS:
        case TOKEN_STAR:
        case TOKEN_AMP:
        default:
            return ast->const_state = CONST_NONE;
    }
}

AstConstState evaluate_constant(Ast *ast)
{
    if (ast == NULL) return CONST_FULL;
    if (ast->const_state != CONST_UNKNOWN) return ast->const_state;
    switch (ast->type)
    {
        case AST_TYPE_EXPR:
            return evaluate_constant_type_expr(ast);
        case AST_FLOAT_EXPR:
        case AST_BOOL_EXPR:
        case AST_INT_EXPR:
        case AST_NIL_EXPR:
        case AST_STRING_EXPR:
        case AST_UINT_EXPR:
            FATAL_ERRORF("Should already be marked CONST_FULL");
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
            FATAL_ERRORF("Unexpected type %d for folding", ast->type);
            return CONST_NONE;
    }
    FATAL_ERRORF("TODO");
    return CONST_UNKNOWN;
}
