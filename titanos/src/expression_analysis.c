//
// Created by Christoffer Lernö on 2019-02-14.
//

#include "expression_analysis.h"
#include "constant_folding.h"
#include "error.h"
#include "type_analysis.h"

bool cast_to_type(Expr *expression, Type *type)
{
    assert(expression->type);
    if (expression->type->type_id == TYPE_CONST_INT && type_is_int(type))
    {
        // Quick test
        expression->type = type;
        return true;
    }
    // TODO
    return false;
}

bool analyse_init_expr(Decl *decl)
{
    evaluate_constant(decl->var.init_expr);
    return cast_to_type(decl->var.init_expr, decl->var.type);
}

Type *try_cast(Expr *a, Expr *b)
{

    DEBUG_LOG("Attempting to cast %.*s & %.*s ", SPLAT_TOK(a->span), SPLAT_TOK(b->span));
    return a->type;
}


bool analyse_binary_expr(Expr *expr, Side side)
{
    LOG_FUNC
    Expr *left = expr->binary_expr.left;
    Expr *right = expr->binary_expr.right;
    bool success = analyse_expr(left, RHS);
    success = analyse_expr(right, RHS) & success;
    if (!success) return false;

    assert(!expr->type);

    switch (expr->binary_expr.operator)
    {
        case TOKEN_EQ:
            expr->type = try_cast(left, right);
            break;
        case TOKEN_MINUS:
        case TOKEN_PLUS:
            expr->type = try_cast(left, right);
            break;
        case TOKEN_DIV:
        case TOKEN_MOD:
        case TOKEN_STAR:
        case TOKEN_EQEQ:
        case TOKEN_NOT_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQ:
        case TOKEN_LESS:
        case TOKEN_LESS_EQ:
        case TOKEN_LEFT_SHIFT:
        case TOKEN_RIGHT_SHIFT:
        case TOKEN_OR:
        case TOKEN_BIT_OR:
        case TOKEN_AND:
        case TOKEN_AMP:
        case TOKEN_BIT_XOR:
        case TOKEN_PLUS_ASSIGN:
        case TOKEN_MINUS_ASSIGN:
        case TOKEN_MULT_ASSIGN:
        case TOKEN_MOD_ASSIGN:
        case TOKEN_DIV_ASSIGN:
        case TOKEN_AND_ASSIGN:
        case TOKEN_BIT_AND_ASSIGN:
        case TOKEN_OR_ASSIGN:
        case TOKEN_BIT_OR_ASSIGN:
        case TOKEN_BIT_XOR_ASSIGN:
        case TOKEN_RIGHT_SHIFT_ASSIGN:
        case TOKEN_LEFT_SHIFT_ASSIGN:
            FATAL_ERROR("TODO");
            break;
        default:
            FATAL_ERROR("Not possible");
    }
    return expr->type != NULL;
}
bool analyse_ternary_expr(Expr *expr, Side side)
{
    FATAL_ERROR("TODO");
}


static bool is_assignable(Expr *expr)
{
    return true;
    /*
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            break;
        case EXPR_CONST:
            break;
        case EXPR_BINARY:
            return is_assignable(expr->binary_expr.left) && is_assignable(expr->binary_expr.right);
        case EXPR_TERNARY:
            return returnis_assignable(expr->ternary_expr.true_expr) && is_assignable(expr->ternary_expr.false_expr);
        case EXPR_UNARY:
            // &x = ... and *x = ... are valid, the rest (-, --, ++, !, ~) are not.
            return expr->unary_expr.operator == TOKEN_STAR || expr->unary_expr.operator == TOKEN_AMP;
            break;
        case EXPR_POST:break;
        case EXPR_IDENTIFIER:break;
        case EXPR_CALL:break;
        case EXPR_SIZEOF:break;
        case EXPR_CAST:break;
        case EXPR_SUBSCRIPT:break;
        case EXPR_ACCESS:break;
        case EXPR_STRUCT_INIT_VALUES:break;
        case EXPR_DESIGNATED_INITIALIZED:break;
    }
    bool FunctionAnalyser::checkAssignee(Expr* expr) const {
        switch (expr->getKind()) {
            case EXPR_INTEGER_LITERAL:
            case EXPR_FLOAT_LITERAL:
            case EXPR_BOOL_LITERAL:
            case EXPR_CHAR_LITERAL:
            case EXPR_STRING_LITERAL:
            case EXPR_NIL:
                break;
            case EXPR_CALL:
            case EXPR_IDENTIFIER:
                // ok
                return true;
            case EXPR_INITLIST:
            case EXPR_DESIGNATOR_INIT:
            case EXPR_TYPE:
                break;
            case EXPR_BINOP:
                // ok
                return true;
            case EXPR_CONDOP:
                break;
            case EXPR_UNARYOP:
                // sometimes... (&)
                return true;
            case EXPR_BUILTIN:
                break;
            case EXPR_ARRAYSUBSCRIPT:
            case EXPR_MEMBER:
            case EXPR_PAREN:
            case EXPR_BITOFFSET:
                // ok
                return true;
            case EXPR_CAST:
                TODO;
                break;
        }
        // expr is not assignable
        // TODO test (also ternary)
        Diag(expr->getLocation(), diag::err_typecheck_expression_not_modifiable_lvalue);
        return false;
    }
*/
}

static bool analyse_type_expr(Expr *expr, Side side)
{
    Type *type = expr->type_expr.type;
    if (!resolve_type(type, false)) return false;
    // Type now resolves, we can set the expression
    Type *type_of_type = new_type(TYPE_TYPEVAL, false, &type->span);
    type_of_type->is_public = type->is_public;
    type_of_type->is_exported = type->is_exported;
    type_of_type->name = type->name;
    type_of_type->module = type->module;
    type_of_type->type_of_type = type;
    return true;
}

static bool resolve_identifier(Expr *expr, Side side)
{
    LOG_FUNC
    Decl *decl = scope_find_symbol(&expr->identifier_expr.identifier, false, false);
    if (!decl)
    {
        return false;
    }
    expr->identifier_expr.resolved = decl;
    decl->is_used = true;
    switch (decl->type_id)
    {
        case DECL_FUNC:
        case DECL_ENUM_CONSTANT:
        case DECL_STRUCT_TYPE:
        case DECL_FUNC_TYPE:
        case DECL_BUILTIN:
        case DECL_ALIAS_TYPE:;
        case DECL_ENUM_TYPE:
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
            if (side == LHS)
            {
                sema_error_at(&expr->span, "Only variables can be assigned to");
                return false;
            }
            expr->const_state = CONST_FULL;
            break;
        case DECL_VAR:
            if (decl->var.in_init)
            {
                sema_error_at(&expr->span, "Used '%.*s' in own initialization", SPLAT_TOK(expr->identifier_expr.identifier));
                return false;
            }
            if (side == LHS && decl->var.type->is_const)
            {
                sema_error_at(&expr->span, "Cannot assign to constant value");
                return false;
            }
            expr->const_state = decl->var.type->is_const ? CONST_FULL : CONST_NONE;
            break;
    }
    if (side == RHS)
    {
        decl->is_used = true;
        // TODO
        // if (usedPublicly) decl->is_used_public = true;
    }
    return expr->const_state;
}

bool analyse_expr(Expr *expr, Side side)
{
    LOG_FUNC
    if (side == LHS)
    {
        if (!is_assignable(expr)) return false;
    }
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            return analyse_type_expr(expr, side);
        case EXPR_CONST:
            return true;
            //return analyse_const_expr(expr, side);
            break;
        case EXPR_BINARY:
            return analyse_binary_expr(expr, side);
        case EXPR_TERNARY:
            return analyse_ternary_expr(expr, side);
        case EXPR_UNARY:
            //return analyse_unary_expr(expr, side);
        case EXPR_POST:
            //return analyse_post_expr(expr, side);
        case EXPR_IDENTIFIER:
            return resolve_identifier(expr, side);
        case EXPR_CALL:
            //return analyse_call(expr, side);
        case EXPR_SIZEOF:
            //return analyse_sizeof(expr, side);
        case EXPR_CAST:
            //return analyse_cast(expr, side);
        case EXPR_SUBSCRIPT:
            //return analyse_subscript(expr, side);
        case EXPR_ACCESS:
            //return analyse_access(expr, side);
        case EXPR_STRUCT_INIT_VALUES:
            //return analyse_struct_init_values(expr, side);
        case EXPR_DESIGNATED_INITIALIZER:
            //return analyse_designated_initializer(expr, side);
            break;
    }
    FATAL_ERROR("Unreachable");
}
