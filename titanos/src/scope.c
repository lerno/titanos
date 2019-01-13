#include "scope.h"
#include "table.h"
#include "ast_types.h"
#include "diagnostics.h"

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


