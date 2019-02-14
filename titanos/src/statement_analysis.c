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
#include "constant_folding.h"
#include "expression_analysis.h"

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
        decl->var.in_init = true;
        success = analyse_init_expr(decl) & success;
        decl->var.in_init = false;
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

bool analyse_global_var(Decl *decl)
{
    assert(decl->type_id == DECL_VAR);
    assert(decl->var.kind == VARDECL_GLOBAL);
    // TODO
    return false;
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
        if (!analyse_expr(r_value, RHS))
        {
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
#ifdef TODOX
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
