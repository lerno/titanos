#include "module.h"
#include "printer.h"
#include "parser.h"
#include "ast_utils.h"

void module_init(Module *module, Token *name, bool is_external, bool is_c_lib)
{
    module->name = *name;
    module->is_c_library = is_c_lib;
    module->is_exported = false;
    module->is_external = is_external;
    table_init(&module->symbols, 128);
    table_init(&module->struct_functions, 8);
    module->files = new_vector(4);
}

/*
const std::string& Module::getCName() const {
    if (m_isCLib) return empty;
    return name;
}
*/



Ast *module_find_symbol(Module *module, Token *symbol)
{
    return table_get_token(&module->symbols, symbol);
}

Ast *module_add_symbol(Module *module, Token *symbol, Ast *value)
{
    assert(symbol->length > 0);
    return table_set_token(&module->symbols, symbol, value);
}

Ast *module_add_struct_function(Module *module, Token *symbol, Ast *value)
{
    return table_set_token(&module->struct_functions, symbol, value);
}

void module_print_files(Module *module)
{
    if (module->files->size == 0) print_color(ANSI_GREY);
    indent(1);
    print_token(&module->name);
    print_color(ANSI_NORMAL);
    printf("\n");
    for (unsigned i = 0; i < module->files->size; i++)
    {
        Parser *parser = module->files->entries[i];
        indent(2);
        printf("%s\n", parser->filename);
    }
}
