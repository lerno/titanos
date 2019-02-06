//
// Created by Christoffer Lernö on 2019-02-01.
//

#include "statement_analysis.h"
#include "common.h"
#include "analyser.h"
#include "diagnostics.h"
#include "error.h"
#include "types/type.h"

enum
{
    RHS,
    LHS
};
static inline bool analyse_while(Ast *stmt)
{
    scope_enter(&analyser->scope, SCOPE_DECL | SCOPE_CONTROL);
    bool success = analyse_stmt(stmt->while_stmt.expr);
    scope_enter(&analyser->scope, SCOPE_BREAK | SCOPE_CONTINUE | SCOPE_DECL);
    success |= analyse_stmt(stmt->while_stmt.body);
    scope_exit(&analyser->scope, stmt->while_stmt.body);
    // Control scope will prevent defer.
    scope_exit(&analyser->scope, NULL);
    return success;
}


bool analyse_compound_stmt(Ast *compound_stmt)
{
    assert(analyser->current_func);
    for (unsigned i = 0; i < compound_stmt->compound_stmt.stmts->size; i++)
    {
        analyse_stmt(compound_stmt->compound_stmt.stmts->entries[i]);
        if (scope_had_errors(&analyser->scope)) return false;
    }
    return true;
}

bool cast_to_type(Ast *expression, Type *type_expression)
{
    return true;
}

bool analyse_return(Ast *stmt)
{
    if (!scope_allow_scope_exit(&analyser->scope))
    {
        sema_error_at(&stmt->span, "Return statement is forbidden in defer");
        return false;
    }
    Ast *r_value = stmt->return_stmt.expr;
    if (r_value)
    {
        if (analyser->current_func->func_decl.rtype->type_id == TYPE_VOID)
        {
            sema_error_at(&r_value->span, "Return value is invalid for void function");
            return false;
        }
        if (!cast_to_type(r_value, analyser->current_func->func_decl.rtype))
        {
            // IMPROVE type
            sema_error_at(&r_value->span, "Return value does not match function return type");
            return false;
        }
    }
    else
    {
        if (analyser->current_func->func_decl.rtype->type_id != TYPE_VOID)
        {
            sema_error_at(&stmt->span, "Expected return value");
            return false;
        }
    }
    stmt->return_stmt.defer_top = scope_defer_top(&analyser->scope);
    return true;
}

bool analyse_stmt(Ast *stmt)
{
    switch (stmt->type)
    {
        case AST_RETURN_STMT:
            return analyse_return(stmt);
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
        case AST_DECL:
            return analyse_decl(stmt);
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
        case STMT_EXPR:
            analyseStmtExpr(S);
            break;
        case STMT_COMPOUND:
            break;
#endif
    }
return false;
}
