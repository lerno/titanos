#pragma once
#include "lexer.h"
#include "vector.h"
typedef struct _Module
{
    Token name;

    bool is_external;
    bool is_c_library;
    bool is_exported;

    Vector *symbols;
    Vector *struct_functions;
    Vector *asts;
    /*Symbols symbols;*
    AttrMap declAttrs;

    Symbols structFuncs;
    AstList files;*/

} Module;

typedef enum
{
    IMPORT_TYPE_FULL,
    IMPORT_TYPE_ALIAS,
    IMPORT_TYPE_LOCAL,
} ImportType;
typedef struct _Import
{
    ImportType type;
    Token span;
    Token module;
    Token alias;
} Import;



