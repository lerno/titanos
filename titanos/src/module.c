#include "module.h"
#include "printer.h"
#include "parser.h"

void module_init(Module *module, const char *name, bool is_external, bool is_c_lib)
{
    module->name = name;
    module->is_c_library = is_c_lib;
    module->is_exported = false;
    module->is_external = is_external;
    stable_init(&module->symbols, 128);
    stable_init(&module->struct_functions, 8);
    module->files = new_vector(4);
}

/*
const std::string& Module::getCName() const {
    if (m_isCLib) return empty;
    return name;
}
*/



Decl *module_find_symbol(Module *module, const char *symbol)
{
    return stable_get(&module->symbols, symbol);
}

Decl *module_add_symbol(Module *module, const char *symbol, Decl *value)
{
    assert(symbol);
    return stable_set(&module->symbols, symbol, value);
}

Decl *module_add_struct_function(Module *module, const char *symbol, Decl *value)
{
    return stable_set(&module->struct_functions, symbol, value);
}

void module_print_files(Module *module)
{
    if (module->files->size == 0) print_color(ANSI_GREY);
    indent(1);
    printf("%s", module->name);
    print_color(ANSI_NORMAL);
    printf("\n");
    for (unsigned i = 0; i < module->files->size; i++)
    {
        Parser *parser = module->files->entries[i];
        indent(2);
        printf("%s\n", parser->filename);
    }
}

void module_print_symbols(Module *module)
{
    indent(1);
    printf("module %s\n", module->name);
    for (unsigned i = 0; i < module->symbols.capacity; i++)
    {
        SEntry *entry = &module->symbols.entries[i];
        if (!entry->key) continue;
        indent(2);
        printf("[%s]\n", entry->key);
        decl_print(entry->value, 3);
    }
}
