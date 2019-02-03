#pragma once

#include "module.h"
#include "lexer.h"
#include "table.h"

#define MAX_SCOPE_DEPTH 15

typedef enum _ScopeFlags
{
    SCOPE_FUNC = 0x01, // Function scope, labels are set here
    SCOPE_BREAK = 0x02, // Inside a while,do,switch,for etc that can have break
    SCOPE_CONTINUE = 0x04, // while, do, for that can have continue
    SCOPE_DECL = 0x08, // Can contain declarations

    /// ControlScope - The controlling scope in a if/switch/while/for statement.
    SCOPE_CONTROL = 0x10, // Controlling scope of if/switch/for

    SCOPE_DEFER = 0x20, // prevent continue and return

    SCOPE_SWITCH = 0x40, // In a switch

    SCOPE_HAS_DECLS = 0x800000, // Set if scope has decls
    SCOPE_HAS_BREAKS = 0x1000000, // Set if there are breaks

} ScopeFlags;

typedef struct _Ast Ast;

typedef struct _DynamicScope
{
    unsigned flags;
    unsigned flags_created;
    Vector *local_decls;
    Ast *last_defer;
    unsigned errors;
} DynamicScope;

typedef struct _Import Import;
typedef struct _Ast Ast;
typedef struct _Vector Vector;

typedef struct _Scope
{
    Module *module;
    DynamicScope *cur_scope;
    DynamicScope scopes[MAX_SCOPE_DEPTH];
    unsigned scope_index;    // first free scope (= count of scopes)
    Table *all_modules;
    Table imported_modules;
    Table symbol_cache;
    Vector *locals; // Module list
} Scope;


void scope_add_import_declaration(Scope *scope, Ast *import);
void scope_init(Scope *scope, const Token *name, Table *modules);
Ast *scope_check_scoped_symbol(Scope *scope, Token *name);
void scope_add_scoped_symbol(Scope *scope, Ast *var_decl);
Module *scope_find_used_module(Scope *scope, Token *name, bool used_public);
Ast *scope_find_symbol(Scope *scope, Token *token, bool is_type, bool used_public);
Ast *scope_find_symbol_in_module(Scope *scope, Token *token, Module *module);
void scope_check_access(Scope *scope, Ast *decl, Token *loc);
void push_defer(Scope *scope, Ast *defer_stmt);
void scope_enter(Scope *scope, unsigned flags);
void scope_exit(Scope *scope, Ast *stmt);
Ast *scope_defer_top(Scope *scope);
Ast **exit_scope_defers(unsigned flags);
static inline void scope_set_has_decls(Scope *scope)
{
    scope->cur_scope->flags |= SCOPE_HAS_DECLS;
}
bool scope_had_errors(Scope *scope);
void scope_set_has_breaks(Scope *breaks);
bool scope_has_error(Scope *scope);
static inline bool scope_allow_break(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_BREAK) != 0;
}
static inline bool scope_allow_continue(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_CONTINUE) != 0;
}
static inline bool scope_has_decls(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_HAS_DECLS) != 0;
}
static inline bool scope_has_breaks(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_HAS_BREAKS) != 0;
}
static inline bool scope_allow_scope_exit(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_DEFER) == 0;
}

static inline bool scope_is_defer(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_DEFER) != 0;
}

static inline bool scope_is_control(Scope *scope)
{
    return (scope->cur_scope->flags & SCOPE_CONTROL) != 0;
}

static inline bool scope_is_external_module(Scope *scope, Module *module)
{
    return module && scope->module != module;
}

Module *scope_find_any_module(Scope *scope, const Token *token);

static Ast *find_own_module(Token *symbol);

#ifdef TODO
    // Modules with local symbols (includes self mod)

    DeferStmt* deferForScopeIndex(unsigned index);

    // used Modules (use <as>)
    std::map<std::string, ImportDecl*> importedModules;

    // all modules
    const Modules& allModules;
    const Module* myModule;

    // Symbol caches
    mutable std::map<const std::string, Decl*> symbolCache;

};
#endif
