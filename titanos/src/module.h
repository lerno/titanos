#pragma once
#include "lexer.h"
#include "vector.h"
#include "table.h"

typedef struct _Module
{
    Token name;

    bool is_external;
    bool is_c_library;
    bool is_exported;

    Vector *files; // Asts
    Table struct_functions;
    Table symbols;
    /**
    AttrMap declAttrs;
*/

} Module;


typedef struct _Ast Ast;

Ast *module_add_symbol(Module *module, Token *symbol, Ast *value);
Ast *module_add_struct_function(Module *module, Token *symbol, Ast *value);
Ast *module_find_symbol(Module *module, Token *symbol);
void module_print_files(Module *module);
void module_init(Module *module, Token *name, bool is_external, bool is_c_lib);
void module_print_symbols(Module *module);
