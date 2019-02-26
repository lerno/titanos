#pragma once

#include <stdio.h>
#include "ansi_color.h"
#include "error.h"

static inline void indent(int indent)
{
    for (int i = 0; i < indent; i++) printf("  ");
}

static inline  void print_indent(const char *string, int current_indent)
{
    indent(current_indent);
    printf("%s", string);
}

static inline void print_color(const char *str)
{
    if (use_colors()) printf("%s", str);
}

static inline  void println_indent(const char *string, int current_indent)
{
    indent(current_indent);
    printf("%s\n", string);
}
