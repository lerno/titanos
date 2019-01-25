#pragma once

#include "common.h"
typedef struct _Vector Vector;

void codegen(const char *filename, bool single_module, Vector *modules);
