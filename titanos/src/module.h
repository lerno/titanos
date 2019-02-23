#pragma once
#include "lexer.h"
#include "vector.h"
#include "decl.h"
#include "symtab.h"

typedef struct _Module
{
    const char *name;

    bool is_external;
    bool is_c_library;
    bool is_exported;

    Vector *files; // Asts
    STable struct_functions;
    STable symbols;
    /**
    AttrMap declAttrs;
*/

} Module;


typedef struct _Ast Ast;

Decl * module_add_symbol(Module *module, const char *symbol, Decl *value);
Decl * module_add_struct_function(Module *module, const char *symbol, Decl *value);
Decl *module_find_symbol(Module *module, const char *symbol);
void module_print_files(Module *module);
void module_init(Module *module, const char *name, bool is_external, bool is_c_lib);
void module_print_symbols(Module *module);
