#include "ast_utils.h"


bool is_negative(Ast *expr)
{
    switch (expr->type)
    {
        case AST_INT_EXPR:
            return expr->int_expr.sign && (int64_t)expr->int_expr.i < 0;
        case AST_BOOL_EXPR:
            return false;
        case AST_FLOAT_EXPR:
            return expr->float_expr.f < 0;
        default:
            assert(false && "Unexpected type here");
            return false;
    }
}
