#include <string.h>
#include <stdio.h>
#include "component.h"
#include "module.h"
#include "arena_allocator.h"
#include "printer.h"
#include "error.h"

Component *component_create(const char *name, const char *path, ComponentType type, bool external, bool c_library, Array *export_list)
{
    Component *component = malloc_arena(sizeof(Component));
    component->name = name;
    component->exports = export_list;
    component->is_c_library = c_library;
    component->is_external = external;
    vector_init(&component->dependencies, 8);
    vector_init(&component->modules, 8);
    return component;
}

const char *component_type_to_string(ComponentType type)
{
    switch (type)
    {
        case COMPONENT_TYPE_EXECUTABLE:
            return "executable";
        case COMPONENT_TYPE_SHARED_LIB:
            return "shared";
        case COMPONENT_TYPE_STATIC_LIB:
            return "static";
        default:
            return "unknown";
    }
}

ComponentType component_type_from_string(const char *string)
{
    static char *c[] = { "executable", "shared", "static" };
    for (ComponentType type = COMPONENT_TYPE_EXECUTABLE; type <= COMPONENT_TYPE_STATIC_LIB; type++)
    {
        if (strcmp(c[type], string) == 0) return type;
    }
    return (ComponentType)-1;
}

void component_print_symbols(Component *component)
{
    printf("Component %s\n", component->name);
    for (unsigned i = 0; i < component->modules.size; i++)
    {
        Module *module = component->modules.entries[i];
        if (!module->files->size) continue;
        module_print_symbols(module);
    }
}

void component_print(Component *component)
{
    printf("Component %s ", component->name);
    print_color(COL_ATTRIBUTES);
    printf("%s", component_type_to_string(component->type));
    if (component->is_external)
    {
        printf(" external");
    }
    if (component->dependencies.size)
    {
        print_color(COL_EXPR);
        printf(" (");
        for (unsigned i = 0; i < component->dependencies.size; i++)
        {
            if (i != 0) printf(", ");
            Component *dep = component->dependencies.entries[i];
            printf("%s %s", dep->name, component_type_to_string(dep->type));
        }
        printf(")");
    }
    print_color(ANSI_NORMAL);
    printf("\n");
    for (int i = 0; i < component->modules.size; i++)
    {
        Module *module = component->modules.entries[i];
        module_print_files(module);


    }
}

bool component_has_dependency(Component *this, const Component* other)
{
    for (unsigned i = 0; i < this->dependencies.size; i++)
    {
        if (this->dependencies.entries[i] == other) return true;
    }
    return false;
}

static bool component_is_exported(Component *component, Token *module_name)
{
    for (unsigned i = 0; i < component->exports->count; i++)
    {
        if (token_compare(module_name, component->exports->entries[i]))
        {
            return true;
        }
    }
    return false;
}

Module *component_get_module(Component *component, Token *token)
{
    for (unsigned i = 0; i < component->modules.size; i++)
    {
        Module *module = component->modules.entries[i];
        if (token_compare(token, &module->name)) return module;
    }
    Module *module = malloc_arena(sizeof(Module));
    module_init(module, token, component->is_external, component->is_c_library);
    module->is_exported = component_is_exported(component, token);
    vector_add(&component->modules, module);
    return module;
}


Module *component_find_module(Component *component, const Token *token)
{
    for (unsigned i = 0; i < component->modules.size; i++)
    {
        Module *module = component->modules.entries[i];
        if (token_compare(&module->name, token)) return module;
    }
    return NULL;
}
