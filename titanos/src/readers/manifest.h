#pragma once

#include "common.h"
#include "vector.h"


typedef struct _ManifestEntry
{
    const char *name;
    const char *c2_file;
} ManifestEntry;

typedef struct _Manifest
{
    const char *filename;
    const char *linkname;
    Vector entries;
    Vector deps;
    bool is_native;
    bool has_static_lib;
    bool has_dynamic_lib;
} Manifest;

bool manifest_parse(const char *filename, Manifest *manifest);
