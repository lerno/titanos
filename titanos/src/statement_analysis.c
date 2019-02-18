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

static inline Vector *defer_stack()
{
    __thread static Vector stack = { .size = 0 };
    return &stack;
}


bool analyse_decl(Decl *decl)
{
    LOG_FUNC

    if (!scope_allow_decl())
    {
        sema_error_at(&decl->span, "Declarations is not allowed here");
        return false;
    }

    scope_set_has_decls();

    assert(decl->type_id == DECL_VAR);
    assert(decl->var.kind == VARDECL_LOCAL);

    // Resolve the type as local (non public)
    bool success = resolve_type(&decl->var.type, false);

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
            sema_error_at(&decl->var.type->array.len_expr->span, "Could not resolve array length");
            success = false;
        }
        if (decl->var.type->array.is_empty && !decl->var.init_expr)
        {
            sema_error_at(&decl->span, "Array without fixed size needs initializer");
            success = false;
        }
    }
    if (decl->var.type->type_id == TYPE_OPAQUE)
    {
        if (scope_is_external_module(decl->var.type->module))
        {
            sema_error_at(&decl->span, "Cannot create opaque type");
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
bool analyse_decl_stmt(Ast *decl_stmt)
{
    LOG_FUNC
    assert(decl_stmt->ast_id == AST_DECLARE_STMT);

    return analyse_decl(decl_stmt->declare_stmt.decl);
}

bool analyse_compound_stmt(Ast *compound_stmt)
{
    assert(active_analyser->current_func);
    Vector *stmts = compound_stmt->compound_stmt.stmts;
    int exit_found = -1;
    for (unsigned i = 0; i < stmts->size; i++)
    {
        Ast *stmt = stmts->entries[i];
        analyse_stmt(stmt);
        if (scope_had_errors()) return false;
        if (exit_found == -1 && stmt->exit != EXIT_NONE)
        {
            exit_found = i;
            compound_stmt->exit = stmt->exit;
        }
    }
    // If we find the exit, remove the last statements even though we analysed them. Possibly we even skip analysis.
    if (exit_found > -1)
    {
        stmts->size = (unsigned)exit_found + 1;
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
    stmt->exit = EXIT_RETURN;
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
        if (!insert_implicit_cast_if_needed(r_value, active_analyser->current_func->func_decl.rtype))
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

static inline bool analyse_break(Ast *stmt)
{
    LOG_FUNC
    if (!scope_allow_break())
    {
        sema_error_at(&stmt->span, "Break is not allowed here");
        return false;
    }
    scope_set_has_breaks();
    stmt->exit = EXIT_BREAK;
//    B->deferList = scope.exitScopeDefers(Scope::BreakScope);
    return true;
}
static inline bool analyse_continue(Ast *stmt)
{
    LOG_FUNC
    if (!scope_allow_continue())
    {
        sema_error_at(&stmt->span, "Unexpected continue found");
        return false;
    }
    stmt->exit = EXIT_CONTINUE;
    // TODO
    // C->deferList = scope.exitScopeDefers(Scope::ContinueScope);
    return true;
}

static inline bool analyse_condition(Ast *stmt)
{
    assert(stmt->ast_id == AST_COND_STMT);
    switch (stmt->cond_stmt.cond_type)
    {
        case COND_DECL:
            if (!analyse_decl(stmt->cond_stmt.decl)) return false;
            // TODO casts!
            FATAL_ERROR("Missing cast code");
            return true;
        case COND_EXPR:
            if (!analyse_expr(stmt->cond_stmt.expr, RHS)) return false;
            return insert_bool_cast_for_conditional_if_needed(stmt->cond_stmt.expr);
    }
}

static inline bool analyse_while(Ast *stmt)
{
    scope_enter(SCOPE_DECL | SCOPE_CONTROL);
    bool success = analyse_condition(stmt->while_stmt.cond);
    scope_enter(SCOPE_BREAK | SCOPE_CONTINUE | SCOPE_DECL);
    success = analyse_stmt(stmt->while_stmt.body) && success;
    stmt->exit = (stmt->while_stmt.body->exit == EXIT_RETURN && !scope_has_breaks() && !scope_has_continues()) ? EXIT_RETURN : EXIT_NONE;
    scope_exit(stmt->while_stmt.body);
    // Control scope will prevent defer.
    scope_exit(NULL);
    return success;
}

static inline bool analyse_do(Ast *stmt)
{
    scope_enter(SCOPE_BREAK | SCOPE_CONTINUE | SCOPE_DECL);
    bool success = analyse_stmt(stmt->do_stmt.body);
    stmt->exit = (stmt->while_stmt.body->exit == EXIT_RETURN && !scope_has_breaks() && !scope_has_continues()) ? EXIT_RETURN : EXIT_NONE;
    scope_exit(stmt->do_stmt.body);
    scope_enter(SCOPE_DECL | SCOPE_CONTROL);
    success = analyse_expr(stmt->do_stmt.expr, RHS) || success;
    scope_exit(NULL);
    return success;
}

static inline bool analyse_case_stmt(Ast *stmt)
{
    return false;
}

static inline bool analyse_default_stmt(Ast *stmt)
{
    return false;
}

static inline bool analyse_if_stmt(Ast *stmt)
{
    assert(stmt->ast_id == AST_IF_STMT);
    scope_enter(SCOPE_DECL);
    analyse_condition(stmt->if_stmt.cond);
    scope_enter(SCOPE_DECL);
    analyse_stmt(stmt->if_stmt.then_body);
    CondValue cond_value = ast_cond_value(stmt->if_stmt.cond);
    scope_exit(stmt->if_stmt.then_body);

    // Set default exit to that of the "then" body – except if the condifition is sure to evaluate to false.
    stmt->exit = cond_value == COND_FALSE ? EXIT_NONE : stmt->if_stmt.then_body->exit;


    if (stmt->if_stmt.else_body)
    {
        scope_enter(SCOPE_DECL);
        analyse_stmt(stmt->if_stmt.else_body);
        scope_exit(stmt->if_stmt.else_body);

        switch (cond_value)
        {
            case COND_TRUE:
                // Else never executed, so exit status is same as then.
                break;
            case COND_FALSE:
                // Always the else, so exit is same as else.
                stmt->exit = stmt->if_stmt.else_body->exit;
                break;
            case COND_VARIABLE:
                // Any result is possible, so exit is the same as min of the two possible exits:
                if (stmt->exit > stmt->if_stmt.else_body->exit)
                {
                    stmt->exit = stmt->if_stmt.else_body->exit;
                }
                break;
        }
    }
    scope_exit(NULL);
    return true;
}

static inline bool analyse_switch(Ast *stmt)
{
    LOG_FUNC
    scope_enter(SCOPE_DECL | SCOPE_CONTROL);
    if (!analyse_condition(stmt->switch_stmt.cond)) return false;
    scope_enter(SCOPE_BREAK | SCOPE_SWITCH);
    for (unsigned i = 0; i < stmt->switch_stmt.case_list->size; i++)
    {
        Ast *case_stmt = stmt->switch_stmt.case_list->entries[i];
        if (!analyse_case_stmt(case_stmt)) return false;
    }
    if (stmt->switch_stmt.default_stmt)
    {
        if (!analyse_default_stmt(stmt->switch_stmt.default_stmt)) return false;
    }
/* TODO
    QualType QT = getConditionType(S->getCond());
    if (const EnumType* ET = dyncast<EnumType>(QT)) {
        checkEnumCases(S, ET);
    }
*/
    // Since we have scopes for each statment, we know this is empty of defers.
    scope_exit(NULL);
    // Since we have control scope, we know this is empty of defers.
    scope_exit(NULL);
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
        case AST_COMPOUND_STMT:
//            if (!haveScope) scope.EnterScope(Scope::DeclScope);
            return analyse_compound_stmt(stmt);
    //        if (!haveScope)
            {
  //              scope.ExitScope(Context, &S);
            }
        case AST_IF_STMT:
            return analyse_if_stmt(stmt);
        case AST_WHILE_STMT:
            return analyse_while(stmt);
        case AST_CONTINUE_STMT:
            return analyse_continue(stmt);
        case AST_DO_STMT:
            return analyse_do(stmt);
        case AST_BREAK_STMT:
            return analyse_break(stmt);
        case AST_SWITCH_STMT:
            return analyse_switch(stmt);
        case AST_CASE_STMT:
        case AST_DEFAULT_STMT:
            // These are handled inside of the switch stmt
            UNREACHABLE
        case AST_COND_STMT:
            // Handled inside of for/while/switch
            UNREACHABLE

#ifdef TODOX
        case AST_FOR_STMT:
            return analyse_for(stmt);
        case AST_LABEL:
            return analyse_label(stmt);
        case AST_GOTO_STMT:
            return analyse_goto(stmt);
        case AST_DEFER_STMT:
            return analyse_defer(stmt);
        case AST_ASM:
            return analyse_asm(stmt);
        case AST_COMPOUND_STMT:
            break;
        case AST_DEFER_RELASE:
        case STMT_COMPOUND:
            break;
#endif
    }
    FATAL_ERROR("Unhandled stmt type %d", stmt->ast_id);
return false;
}
