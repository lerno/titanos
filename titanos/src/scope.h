#pragma once

#include "module.h"
#include "lexer.h"
#include "table.h"
#include "static_array.h"

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
    SCOPE_HAS_CONTINUE = 0x2000000, // Set if there are continues

} ScopeFlags;

typedef struct _Ast Ast;

typedef struct _DynamicScope
{
    unsigned flags;
    unsigned flags_created;
    Vector *local_decls;
    Ast *last_defer;
    unsigned errors;
    Ast *active_defer;
} DynamicScope;

typedef struct _Import Import;
typedef struct _Ast Ast;
typedef struct _Vector Vector;

#define MAX_LOCALS 0xFFFF
typedef struct _Scope
{
    Module *module;
    DynamicScope *cur_scope;
    DynamicScope scopes[MAX_SCOPE_DEPTH];
    unsigned scope_index;    // first free scope (= count of scopes)
    STable *all_modules;
    STable imported_modules;
    STable symbol_cache;
    STATIC_ARRAY(Module *, MAX_LOCALS) locals; // Module list
} Scope;

extern __thread Scope *active_scope;
void scope_init(Scope *scope, const char *name, STable *modules);
void scope_reset(Scope *scope, const char *name, STable *modules);


void scope_add_import_declaration(Decl *import);
Decl *scope_check_scoped_symbol(const char *symbol);
void scope_add_scoped_symbol(Decl *var_decl);
Module *scope_find_used_module(const char *name, SourceRange span, bool used_public);
Decl *scope_find_symbol(const char *symbol, bool is_type, bool used_public, SourceRange span);
Decl *scope_find_symbol_in_module(Module *module, const char *symbol, SourceRange span);
void scope_check_access(Ast *decl, Token *loc);
void scope_push_defer(Ast *defer_stmt);
void scope_enter(unsigned flags);
void scope_exit(Ast *stmt);
void scope_set_defer(Ast *defer_stmt);
Ast *scope_active_defer(void);
Ast *scope_defer_top();
Ast **exit_scope_defers(unsigned flags);
static inline void scope_set_has_decls()
{
    active_scope->cur_scope->flags |= SCOPE_HAS_DECLS;
}
bool scope_had_errors(void);
static inline void scope_set_has_breaks(void)
{
    active_scope->cur_scope->flags |= SCOPE_HAS_BREAKS;
}

static inline void scope_set_has_continues(void)
{
    active_scope->cur_scope->flags |= SCOPE_HAS_CONTINUE;
}

bool scope_has_error(void);
static inline bool scope_allow_break()
{
    return (active_scope->cur_scope->flags & SCOPE_BREAK) != 0;
}
static inline bool scope_allow_decl()
{
    return (active_scope->cur_scope->flags & SCOPE_DECL) != 0;
}
static inline bool scope_allow_continue()
{
    return (active_scope->cur_scope->flags & SCOPE_CONTINUE) != 0;
}
static inline bool scope_has_decls()
{
    return (active_scope->cur_scope->flags & SCOPE_HAS_DECLS) != 0;
}
static inline bool scope_has_breaks()
{
    return (active_scope->cur_scope->flags & SCOPE_HAS_BREAKS) != 0;
}
static inline bool scope_has_continues()
{
    return (active_scope->cur_scope->flags & SCOPE_HAS_CONTINUE) != 0;
}
static inline bool scope_allow_scope_exit()
{
    return (active_scope->cur_scope->flags & SCOPE_DEFER) == 0;
}

static inline bool scope_is_defer()
{
    return (active_scope->cur_scope->flags & SCOPE_DEFER) != 0;
}

static inline bool scope_is_control()
{
    return (active_scope->cur_scope->flags & SCOPE_CONTROL) != 0;
}

static inline bool scope_is_external_module(Module *module)
{
    return module && active_scope->module != module;
}

Module *scope_find_any_module(const char *token);

static Ast *find_own_module(Token *symbol);

#ifdef TODOX
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
