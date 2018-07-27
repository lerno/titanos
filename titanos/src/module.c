#include "module.h"
#include "printer.h"

void init_module(Module *module, Token *name, bool is_external, bool is_c_lib)
{
    module->name = *name;
    module->is_c_library = is_c_lib;
    module->is_exported = false;
    module->is_external = is_external;
    module->symbols = new_vector(8);
    module->struct_functions = new_vector(8);
}

/*
const std::string& Module::getCName() const {
    if (m_isCLib) return empty;
    return name;
}
*/


