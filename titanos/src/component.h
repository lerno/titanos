#pragma once

#include "lexer.h"
#include "array.h"
#include "vector.h"
typedef enum
{
    COMPONENT_TYPE_EXECUTABLE,
    COMPONENT_TYPE_SHARED_LIB,
    COMPONENT_TYPE_STATIC_LIB,
} ComponentType;

typedef struct _Module Module;

typedef struct _Component
{
    const char *name; // eg libc/pthread
    const char *path; // for external Components, points to dir containing manifest
    const char *link_name; // used for external libs (-l..)
    bool is_external;
    bool is_c_library;
    Vector modules;
    Array *exports; // string list
    Vector dependencies; // Component *[]
    ComponentType type;
} Component;


ComponentType component_type_from_string(const char *string);
const char *component_type_to_string(ComponentType type);
void component_print(Component *component);
Component *component_create(const char *name, const char *path, ComponentType type, bool external, bool c_library, Array *export_list);
bool component_has_dependency(Component *this, const Component* other);
Module *component_find_module(Component *component, const Token *name);
Module *component_get_module(Component *component, Token *token);
