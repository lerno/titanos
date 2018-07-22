#include <string.h>
#include "component.h"
#include "module.h"
#include "arena_allocator.h"


Component *component_create(const char *name, const char *path, ComponentType type, bool external, bool c_library, Array *export_list)
{
    Component *component = malloc(sizeof(Component *));
    component->name = name;
    component->exports = export_list;
    component->is_c_library = c_library;
    component->is_external = external;
    array_init(&component->dependencies);
    array_init(&component->modules);
    return component;
}
void component_destroy(Component *component)
{
    array_free(&component->dependencies);
    array_free(&component->modules);
    free(component);
}

bool component_has_dependency(Component *this, const Component* other)
{
    for (unsigned i = 0; i < this->dependencies.count; i++)
    {
        if (this->dependencies.entries[i] == other) return true;
    }
    return false;
}

/*
static bool component_is_exported(Component *component, Token *module_name)
{
    for (unsigned i = 0; i < component->exports->size; i++)
    {
        if (token_compare(module_name, component->exports->data[i]))
        {
            return true;
        }
    }
    return false;
}

Module *component_get_module(Component *component, Token *token)
{
    for (unsigned i = 0; i < component->modules->size; i++)
    {
        Module *module = component->modules->data[i];
        if (token_compare(token, &module->name)) return module;
    }
    Module *module = malloc_arena(sizeof(Module));
    module->name = *token;
    module->is_external = component->is_external;
    module->is_c_library = component->is_c_library;
    module->asts = new_vector(4);
    module->is_exported = component_is_exported(component, token);
    vector_add(component->modules, module);
    return module;
}
*/
