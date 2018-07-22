#include "component.h"

typedef struct _Dependency
{
    Component *component;
    ComponentType type;
    const char *name;
} Dependency;
