//
// Created by Christoffer Lernö on 2019-02-01.
//

#include "statement_analysis.h"
#include "common.h"
#include "analyser.h"
#include "diagnostics.h"
#include "error.h"
#include "types/type.h"
#include "type_analysis.h"
#include "expr.h"


typedef enum
{
    RHS,
    LHS
} Side;

static bool analyse_expr(Expr *expr, Side side);

static inline bool analyse_while(Ast *stmt)
{
    scope_enter(SCOPE_DECL | SCOPE_CONTROL);
    bool success = analyse_expr(stmt->while_stmt.expr, RHS);
    scope_enter(SCOPE_BREAK | SCOPE_CONTINUE | SCOPE_DECL);
    success |= analyse_stmt(stmt->while_stmt.body);
    scope_exit(stmt->while_stmt.body);
    // Control scope will prevent defer.
    scope_exit(NULL);
    return success;
}

bool analyse_decl_stmt(Ast *decl_stmt)
{
    LOG_FUNC
    assert(decl_stmt->ast_id == AST_DECLARE_STMT);

    scope_set_has_decls();

    Decl *decl = decl_stmt->declare_stmt.decl;
    assert(decl->type_id == DECL_VAR);
    assert(decl->var.kind == VARDECL_LOCAL);

    // Resolve the type as local (non public)
    bool success = resolve_type(decl->var.type, false);

    if (success && !active_analyser->parser->is_interface)
    {
        if (!is_lower(&decl->name))
        {
            sema_error_at(&decl->name, "Variables should start with lower case");
            success = false;
        }
    }

    if (decl->var.type->type_id == TYPE_ARRAY)
    {
        if (!decl->var.type->array.is_len_resolved)
        {
            sema_error_at(&decl->var.type->span, "Could not resolve array length");
            success = false;
        }
        if (decl->var.type->array.is_empty && !decl->var.init_expr)
        {
            sema_error_at(&decl->var.type->span, "Array without fixed size needs initializer");
            success = false;
        }
    }
    if (decl->var.type->type_id == TYPE_OPAQUE)
    {
        if (scope_is_external_module(decl->var.type->module))
        {
            sema_error_at(&decl->var.type->span, "Cannot create opaque type");
            success = false;
        }
    }

    // check name
    Decl *shadowed = scope_check_scoped_symbol(&decl->name);
    if (shadowed)
    {
        sema_error_at(&decl->name, "Declaration of '%.*s' shadows a previous symbol", SPLAT_TOK(decl->name));
        prev_at(&decl->name, "Shadowed declaration");
        return false;
    }

    if (decl->var.init_expr)
    {
        // TODO
        //success = analyse_init_expr(decl->var.init_expr) & success;
    }

    if (decl->var.type->is_const && !decl->var.init_expr)
    {
        sema_error_at(&decl->span, "Declaration of constant '%.*s' is missing initial value", SPLAT_TOK(decl->name));
        success = false;
    }
    scope_add_scoped_symbol(decl);
    return success;
}

bool analyse_compound_stmt(Ast *compound_stmt)
{
    assert(active_analyser->current_func);
    for (unsigned i = 0; i < compound_stmt->compound_stmt.stmts->size; i++)
    {
        analyse_stmt(compound_stmt->compound_stmt.stmts->entries[i]);
        if (scope_had_errors()) return false;
    }
    return true;
}

bool cast_to_type(Expr *expression, Type *type_expression)
{
    return true;
}

bool analyse_return(Ast *stmt)
{
    if (!scope_allow_scope_exit())
    {
        sema_error_at(&stmt->span, "Return statement is forbidden in defer");
        return false;
    }
    Expr *r_value = stmt->return_stmt.expr;
    if (r_value)
    {
        if (active_analyser->current_func->func_decl.rtype->type_id == TYPE_VOID)
        {
            sema_error_at(&r_value->span, "Return value is invalid for void function");
            return false;
        }
        if (!cast_to_type(r_value, active_analyser->current_func->func_decl.rtype))
        {
            // IMPROVE type
            sema_error_at(&r_value->span, "Return value does not match function return type");
            return false;
        }
    }
    else
    {
        if (active_analyser->current_func->func_decl.rtype->type_id != TYPE_VOID)
        {
            sema_error_at(&stmt->span, "Expected return value");
            return false;
        }
    }
    stmt->return_stmt.defer_top = scope_defer_top();
    return true;
}

bool analyse_binary_expr(Expr *expr, Side side)
{
    LOG_FUNC
    bool success = analyse_expr(expr->binary_expr.left, LHS);
    success = analyse_expr(expr->binary_expr.right, RHS);
    switch (expr->binary_expr.operator)
    {
        case TOKEN_EQ:
        case TOKEN_MINUS:
        case TOKEN_PLUS:
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
        case TOKEN_RIGHT_SHIFT_LOGIC:
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
        case TOKEN_RIGHT_SHIFT_LOGIC_ASSIGN:
        case TOKEN_LEFT_SHIFT_ASSIGN:
            FATAL_ERROR("TODO");
            break;
        default:
            FATAL_ERROR("Not possible");
    }
    FATAL_ERROR("TODO");
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
static bool analyse_expr(Expr *expr, Side side)
{
    LOG_FUNC
    if (side == LHS)
    {
        if (!is_assignable(expr)) return false;
    }
    bool success;
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            //return analyse_type_expr(expr, side);
        case EXPR_CONST:
            //return analyse_const_expr(expr, side);
        case EXPR_BINARY:
            return analyse_binary_expr(expr, side);
        case EXPR_TERNARY:
            return analyse_ternary_expr(expr, side);
        case EXPR_UNARY:
            //return analyse_unary_expr(expr, side);
        case EXPR_POST:
            //return analyse_post_expr(expr, side);
        case EXPR_IDENTIFIER:
            //return analyse_identifier(expr, side);
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

bool analyse_stmt(Ast *stmt)
{
    LOG_FUNC
    switch (stmt->ast_id)
    {
        case AST_RETURN_STMT:
            return analyse_return(stmt);
        case AST_DECLARE_STMT:
            return analyse_decl_stmt(stmt);
        case AST_EXPR_STMT:
            return analyse_expr(stmt->expr_stmt.expr, RHS);
#ifdef TODO
        case AST_IF_STMT:
            return analyse_if(stmt);
        case AST_WHILE_STMT:
            return analyse_while(stmt);
        case AST_DO_STMT:
            return analyse_do(stmt);
        case AST_FOR_STMT:
            return analyse_for(stmt);
        case AST_SWITCH_STMT:
            return analyse_switch(stmt);
        case AST_BREAK_STMT:
            return analyse_break(stmt);
        case AST_BREAK_STMT:
            return analyse_continue(stmt);
        case AST_LABEL:
            return analyse_label(stmt);
        case AST_GOTO_STMT:
            return analyse_goto(stmt);
        case AST_DEFER_STMT:
            return analyse_defer(stmt);
        case AST_ASM:
            return analyse_asm(stmt);
        case AST_COMPOUND_STMT:
            if (!haveScope) scope.EnterScope(Scope::DeclScope);
            analyseCompoundStmt(S);
            if (!haveScope)
            {
                scope.ExitScope(Context, &S);
            }
            break;
        case AST_DEFER_RELASE:
        case AST_CASE_STMT:
        case AST_DEFAULT_STMT:
            FATAL_ERROR("Unreachable");
            break;
        case STMT_COMPOUND:
            break;
#endif
    }
    FATAL_ERROR("Unhandled stmt type %d", stmt->ast_id);
return false;
}
