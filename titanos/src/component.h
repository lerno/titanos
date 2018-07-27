#pragma once

#include "lexer.h"
#include "array.h"

typedef enum
{
    COMPONENT_TYPE_EXECUTABLE,
    COMPONENT_TYPE_SHARED_LIB,
    COMPONENT_TYPE_STATIC_LIB,
} ComponentType;



typedef struct _Component
{
    const char *name; // eg libc/pthread
    const char *path; // for external Components, points to dir containing manifest
    const char *link_name; // used for external libs (-l..)
    bool is_external;
    bool is_c_library;
    Array modules;
    Array *exports; // string list
    Array dependencies;
    // Type type // WTF?!
} Component;


ComponentType component_type_from_string(const char *string);

Component *component_create(const char *name, const char *path, ComponentType type, bool external, bool c_library, Array *export_list);
void component_destroy(Component *component);
bool component_has_dependency(Component *this, const Component* other);

struct _Module *component_get_module(Component *component, Token *token);
