#include "scope.h"
#include "table.h"
#include "ast_types.h"
#include "diagnostics.h"
#include "error.h"

__thread Scope *active_scope = NULL;

void scope_init(Scope *scope, const Token *name, Table *modules)
{
    scope->scope_index = 0;
    scope->cur_scope = NULL;
    scope->all_modules = modules;
    scope->module = table_get_token(scope->all_modules, name);
    scope->locals = new_vector(8);
    table_init(&scope->imported_modules, 32);
    table_init(&scope->symbol_cache, 16 * 1024);
    assert(scope->module);
}

Module *scope_find_any_module(const Token *token)
{
    return table_get_token(active_scope->all_modules, token);
}

void scope_add_import_declaration(Decl *import)
{
    assert(import->module && "Module missing");

    Token *name = &import->name;
    DEBUG_LOG("Importing %.*s", SPLAT_TOK(*name));

    switch (import->import.type)
    {
        case IMPORT_TYPE_LOCAL:
            vector_add(active_scope->locals, import->module);
            break;
        case IMPORT_TYPE_ALIAS:
            name = &import->import.alias;
            break;
        case IMPORT_TYPE_FULL:
            break;
    }

    // Add module to imports.
    void *previous = table_set_token(&active_scope->imported_modules, name, import);
    assert(!previous && "Previous not expected here");


    // Link the name to the declaration in the symbol cache.
    previous = table_set_token(&active_scope->symbol_cache, name, import);
    assert(!previous && "Previous not expected here");
}

void scope_mark_import_as_used(Module *module, bool used_public)
{

    // IMPROVE cache this
    for (unsigned i = 0; i < active_scope->imported_modules.capacity; i++)
    {
        Entry *entry = &active_scope->imported_modules.entries[i];
        if (entry->key == NULL) continue;
        Decl *import = entry->value;
        if (module == import->module)
        {
            import->is_used = true;
            import->is_used_public = true;
            break;
        }
    }
}

Decl *scope_find_symbol_in_module(Token *token, Module *module)
{
    Decl *symbol = module_find_symbol(module, token);
    if (!symbol)
    {
        sema_error_at(token, "Cannot find %.*s in module %.*s", SPLAT_TOK(*token), SPLAT_TOK(module->name));
        return NULL;
    }
    if (scope_is_external_module(module))
    {
        if (!symbol->is_public)
        {
            sema_error_at(token, "%.*s.%.*s is not public", SPLAT_TOK(module->name), SPLAT_TOK(*token));
            return NULL;
        }
        symbol->is_used_public = true;
    }
    return symbol;
}

Decl *scope_find_symbol(Token *symbol, bool is_type, bool used_public)
{
    Decl *symbol_decl = table_get_token(&active_scope->symbol_cache, symbol);
    if (symbol_decl)
    {
        if (used_public && symbol_decl->module != active_scope->module)
        {
            scope_mark_import_as_used(symbol_decl->module, used_public);
        }
        return symbol_decl;
    }
    bool ambiguous = false;
    bool visible_match = false;
    for (unsigned i = 0; i < active_scope->locals->size; i++)
    {
        Module *module = active_scope->locals->entries[i];
        Decl *decl = module_find_symbol(module, symbol);
        if (!decl) continue;
        bool visible = !module->is_external || decl->is_public;
        if (symbol_decl)
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
        symbol_decl = decl;
        visible_match = visible;
    }
    if (ambiguous) return NULL;

    if (!symbol_decl)
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
    scope_mark_import_as_used(symbol_decl->module, used_public);
    if (!visible_match)
    {
        sema_error_at(symbol, "Symbol '%.*s' is not public", symbol->length, symbol->start);
        return NULL;
    }

    if (symbol_decl->module && symbol_decl->module != active_scope->module)
    {
        symbol_decl->is_used_public = true;
    }
    table_set_token(&active_scope->symbol_cache, symbol, symbol_decl);
    return symbol_decl;
}

bool scope_had_errors(void)
{
    unsigned errors_at_scope_start = active_scope->cur_scope ? active_scope->cur_scope->errors : 0;
    return errors_at_scope_start < errors();
}

void scope_add_scoped_symbol(Decl *var_decl)
{
    assert(active_scope->cur_scope);

    vector_add(active_scope->cur_scope->local_decls, var_decl);
    table_set_token(&active_scope->symbol_cache, &var_decl->name, var_decl);
}

Module *scope_find_used_module(Token *name, bool used_public)
{
    Decl *import_decl = table_get_token(&active_scope->imported_modules, name);
    if (import_decl)
    {
        import_decl->is_used = true;
        if (used_public) import_decl->is_used_public = true;
        return import_decl->module;
    }

    // check if used with alias (then fullname is forbidden)
    /* TODO when understood!
    for ()
    for (ImportsConstIter iter2 = importedModules.begin(); iter2 != importedModules.end(); ++iter2) {
        ImportDecl* I = iter2->second;
        I->setUsed();
        if (usedPublic) {
            // TODO refactor common code
            // TODO check if using non-exported module from exported one
            I->setUsedPublic();
        }
        const Module* p = I->getModule();
        if (p->getName() == name) {
            Diags.Report(loc, diag::err_module_has_alias) << name << iter2->first;
            return 0;
        }
    }
    const Module* P2 = findAnyModule(name.c_str());
    if (P2) {
        Diags.Report(loc, diag::err_module_not_used) << name;
    } else {
        Diags.Report(loc, diag::err_unknown_module) << name;
    }
    return 0;*/
    return NULL;
}

Decl *scope_check_scoped_symbol(Token *token)
{
    Decl *old = table_get_token(&active_scope->symbol_cache, token);

    if (old) return old;

    // Check in local modules
    for (unsigned i = 0; i < active_scope->locals->size; i++)
    {
        Module *module = active_scope->locals->entries[i];
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
    switch (stmt->ast_id)
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

void scope_enter(unsigned flags)
{
    if (active_scope->scope_index >= MAX_SCOPE_DEPTH) FATAL_ERROR("Out of scopes");
    DynamicScope *parent = active_scope->cur_scope;
    active_scope->cur_scope = &active_scope->scopes[active_scope->scope_index++];

    active_scope->cur_scope->errors = errors();
    if (!active_scope->cur_scope->local_decls)
    {
        active_scope->cur_scope->local_decls = new_vector(64);
    }
    else
    {
        active_scope->cur_scope->local_decls->size = 0;
    }
    active_scope->cur_scope->flags = flags;
    active_scope->cur_scope->flags_created = flags;
    active_scope->cur_scope->last_defer = NULL;

    if (parent)
    {
        // Keep the break/continue/defer flags.
        active_scope->cur_scope->flags |= (parent->flags & (SCOPE_BREAK | SCOPE_CONTINUE | SCOPE_DEFER));
    }
    if (flags & SCOPE_DEFER)
    {
        // Continue is not valid when entering Defer.
        active_scope->cur_scope->flags &= ~SCOPE_CONTINUE;
    }
}

void scope_exit(Ast *stmt)
{
    DynamicScope *current = active_scope->cur_scope;
    bool had_errors = scope_had_errors();
    for (unsigned i = 0; i < current->local_decls->size; i++)
    {
        Decl *decl = current->local_decls->entries[i];
        assert(decl->type_id == DECL_VAR);
        if (!decl->is_used && !had_errors)
        {
            if (decl->var.kind == VARDECL_PARAM)
            {
                sema_warn_at(DIAG_UNUSED_PARAMETER, &decl->span, "Unused parameter");
            }
            else
            {
                sema_warn_at(DIAG_UNUSED_VARIABLE, &decl->span, "Unused variable");
            }
        }
        // IMPROVE this assumes shadowing is disallowed.
        table_delete_token(&active_scope->symbol_cache, &decl->name);
    }

    Ast *defer = scope_defer_top(active_scope);

    active_scope->scope_index--;
    active_scope->cur_scope = active_scope->scope_index ? &active_scope->scopes[active_scope->scope_index] : NULL;

    Ast *defer_end = scope_defer_top(active_scope);

    if (defer == defer_end) return;

    DeferList list = { .defer_start = defer, .defer_end = defer_end };
    switch (stmt->ast_id)
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
    Ast *old_stmt = new_ast(stmt->ast_id);
    *old_stmt = *stmt;
    stmt->ast_id = AST_DEFER_RELASE;
    stmt->defer_release_stmt.inner = old_stmt;
    stmt->defer_release_stmt.list = list;
}
