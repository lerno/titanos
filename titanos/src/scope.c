#include "scope.h"
#include "table.h"
#include "ast_types.h"
#include "diagnostics.h"
#include "error.h"

void scope_init(Scope *scope, const Token *name, Table *modules)
{
    scope->scope_index = 0;
    scope->cur_scope = NULL;
    scope->all_modules = modules;
    scope->module = NULL;
    scope->module = scope_find_any_module(scope, name);
    scope->locals = new_vector(8);
    table_init(&scope->imported_modules, 32);
    table_init(&scope->symbol_cache, 16 * 1024);
    assert(scope->module);
}

Module *scope_find_any_module(Scope *scope, const Token *token)
{
    return table_get_token(scope->all_modules, token);
}

void scope_add_import_declaration(Scope *scope, Ast *import)
{
    assert(import->import.module && "Module missing");

    Token *name = &import->import.module_name;
    switch (import->import.type)
    {
        case IMPORT_TYPE_LOCAL:
            vector_add(scope->locals,import->import.module);
            break;
        case IMPORT_TYPE_ALIAS:
            name = &import->import.alias;
            break;
        case IMPORT_TYPE_FULL:
            break;
    }

    // Add module to imports.
    void *previous = table_set_token(&scope->imported_modules, name, import);
    assert(!previous && "Previous not expected here");


    // Link the name to the declaration in the symbol cache.
    previous = table_set_token(&scope->symbol_cache, name, import);
    assert(!previous && "Previous not expected here");
}

void scope_mark_import_as_used(Scope *scope, Module *module, bool used_public)
{
    // IMPROVE cache this
    for (unsigned i = 0; i < scope->imported_modules.capacity; i++)
    {
        Entry *entry = &scope->imported_modules.entries[i];
        if (entry->key == NULL) continue;
        Ast *import = entry->value;
        if (module == import->import.module)
        {
            import->import.used = true;
            import->import.used_public = true;
            break;
        }
    }
}

Ast *scope_find_symbol(Scope *scope, Token *symbol, bool is_type, bool used_public)
{
    Ast *ast = table_get_token(&scope->symbol_cache, symbol);
    if (ast)
    {
        if (used_public && ast->definition.module != scope->module)
        {
            scope_mark_import_as_used(scope, ast->import.module, used_public);
        }
        return ast;
    }
    bool ambiguous = false;
    bool visible_match = false;
    for (unsigned i = 0; i < scope->locals->size; i++)
    {
        Module *module = scope->locals->entries[i];
        Ast *decl = module_find_symbol(module, symbol);
        if (!decl) continue;
        bool visible = !module->is_external || decl->definition.is_public;
        if (ast)
        {
            // if previous result was non-visible, replace with new one
            if (visible_match == visible)
            {
                if (!ambiguous)
                {
                    sema_error_at(symbol, "Ambiguous symbol '%.*s'", symbol->length, symbol->start);
                    sema_error_at(&decl->span, "Also found here");
                    ambiguous = true;
                }
                continue;
            }
            if (visible_match) continue;
        }
        ast = decl;
        visible_match = visible;
    }
    if (ambiguous) return NULL;

    if (!ast)
    {
        if (is_type)
        {
            sema_error_at(symbol, "Unknown type '%.*s'", symbol->length, symbol->start);
        }
        else
        {
            sema_error_at(symbol, "Unknown variable '%.*s'", symbol->length, symbol->start);
        }
        return NULL;
    }


    // mark import as as used
    scope_mark_import_as_used(scope, ast->import.module, used_public);
    if (!visible_match)
    {
        sema_error_at(symbol, "Symbol '%.*s' is not public", symbol->length, symbol->start);
        return NULL;
    }

    if (ast->definition.module && ast->definition.module != scope->module)
    {
        ast->definition.is_used_public = true;
    }
    table_set_token(&scope->symbol_cache, symbol, ast);
    return ast;
}

bool scope_had_errors(Scope *scope)
{
    unsigned errors_at_scope_start = scope->cur_scope ? scope->cur_scope->errors : 0;
    return errors_at_scope_start < errors();
}

void scope_add_scoped_symbol(Scope *scope, Ast *var_decl)
{
    assert(scope->cur_scope);
    assert(var_decl->type == AST_DECL);

    vector_add(scope->cur_scope->local_decls, var_decl);
    table_set_token(&scope->symbol_cache, &var_decl->decl.name, var_decl);
}

Ast *scope_check_scoped_symbol(Scope *scope, Token *token)
{
    Ast *old = table_get_token(&scope->symbol_cache, token);

    if (old) return old;

    // Check in local modules
    for (unsigned i = 0; i < scope->locals->size; i++)
    {
        Module *module = scope->locals->entries[i];
        old = module_find_symbol(module, token);
        if (old) return old;
    }

    return NULL;
}

Ast *scope_defer_top(Scope *scope)
{
    for (unsigned i = scope->scope_index; i > 0; i--)
    {
        if (scope->scopes[i - 1].last_defer)
        {
            return scope->scopes[i - 1].last_defer;
        }
    }
    return NULL;
}

static inline bool should_skip_exit_defer_stmt(Ast *stmt)
{
    if (!stmt) return false;
    switch (stmt->type)
    {
        case AST_RETURN_STMT:
        case AST_BREAK_STMT:
        case AST_CONTINUE_STMT:
        case AST_GOTO_STMT:
            return true;
        case AST_COMPOUND_STMT:
            return should_skip_exit_defer_stmt(ast_compound_stmt_last(stmt));
        default:
            return false;
    }
}

void scope_enter(Scope *scope, unsigned flags)
{
    if (scope->scope_index >= MAX_SCOPE_DEPTH) FATAL_ERROR("Out of scopes");
    DynamicScope *parent = scope->cur_scope;
    scope->cur_scope = &scope->scopes[scope->scope_index++];

    scope->cur_scope->errors = errors();
    if (!scope->cur_scope->local_decls)
    {
        scope->cur_scope->local_decls = new_vector(64);
    }
    else
    {
        scope->cur_scope->local_decls->size = 0;
    }
    scope->cur_scope->flags = flags;
    scope->cur_scope->flags_created = flags;
    scope->cur_scope->last_defer = NULL;

    if (parent)
    {
        // Keep the break/continue/defer flags.
        scope->cur_scope->flags |= (parent->flags & (SCOPE_BREAK | SCOPE_CONTINUE | SCOPE_DEFER));
    }
    if (flags & SCOPE_DEFER)
    {
        // Continue is not valid when entering Defer.
        scope->cur_scope->flags &= ~SCOPE_CONTINUE;
    }
}

void scope_exit(Scope *scope, Ast *stmt)
{
    DynamicScope *current = scope->cur_scope;
    bool had_errors = scope_had_errors(scope);
    for (unsigned i = 0; i < current->local_decls->size; i++)
    {
        Ast *decl = current->local_decls->entries[i];
        assert(decl->type == AST_DECL);
        if (!decl->decl.is_used && !had_errors)
        {
            if (decl->decl.is_parameter)
            {
                sema_warn_at(DIAG_UNUSED_PARAMETER, &decl->span, "Unused parameter");
            }
            else
            {
                sema_warn_at(DIAG_UNUSED_VARIABLE, &decl->span, "Unused variable");
            }
        }
        // IMPROVE this assumes shadowing is disallowed.
        table_delete_token(&scope->symbol_cache, &decl->decl.name);
    }

    Ast *defer = scope_defer_top(scope);

    scope->scope_index--;
    scope->cur_scope = scope->scope_index ? &scope->scopes[scope->scope_index] : NULL;

    Ast *defer_end = scope_defer_top(scope);

    if (defer == defer_end) return;

    DeferList list = { .defer_start = defer, .defer_end = defer_end };
    switch (stmt->type)
    {
        case AST_COMPOUND_STMT:
            // Ignore defer if the last statement was an exit.
            if (should_skip_exit_defer_stmt(ast_compound_stmt_last(stmt))) return;
            stmt->compound_stmt.defer_list = list;
            return;
        case AST_CASE_STMT:
            stmt->case_stmt.defer_list = list;
            return;
        case AST_DEFAULT_STMT:
            stmt->default_stmt.defer_list = list;
            return;
        default:
            break;
    }
    Ast *old_stmt = new_ast(stmt->type);
    *old_stmt = *stmt;
    stmt->type = AST_DEFER_RELASE;
    stmt->defer_release_stmt.inner = old_stmt;
    stmt->defer_release_stmt.list = list;
}
