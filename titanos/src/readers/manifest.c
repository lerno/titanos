
#include <string.h>
#include "manifest.h"
#include "toml.h"
#include "common.h"
#include "error.h"
#include "arena_allocator.h"
#include "dependecy.h"

typedef enum
{
    DYNAMIC,
    STATIC,
    UNKNOWN
} CompType;

static bool str_eq(TomlString *toml_str, const char *str)
{
    size_t len = strlen(str);
    if (toml_str->len != len) return false;
    return memcmp(toml_str->str, str, len) == 0;
}

static char *copy_string(TomlString *toml_str)
{
    if (toml_str->len == 0) return "";
    char *string = malloc_arena(toml_str->len + 1);
    strncpy(string, toml_str->str, toml_str->len);
    string[toml_str->len] = '\0';
    return string;
}

static inline CompType component_type_from_toml(TomlString *str)
{
    if (str_eq(str, "static")) return STATIC;
    if (str_eq(str, "dynamic")) return DYNAMIC;
    return UNKNOWN;
}

static bool parse_library(TomlValue *value, const char *filename, Manifest *manifest)
{
    if (value->type != TOML_TABLE)
    {
        PRINT_ERROR("%s library was not a section", filename);
        return false;
    }
    TomlTable *lib_table = value->value.table;
    value = toml_table_get(lib_table, "language");
    if (!value)
    {
        PRINT_ERROR("%s missing library.language", filename);
        return false;
    }
    if (value->type != TOML_STRING)
    {
        PRINT_ERROR("%s library.language not string", filename);
        return false;
    }
    manifest->is_native = str_eq(value->value.string, "C2");

    value = toml_table_get(lib_table, "linkname");
    if (value)
    {
        if (value->type != TOML_STRING)
        {
            PRINT_ERROR("%s library.linkname not string", filename);
            return false;
        }
        manifest->linkname = copy_string(value->value.string);
    }
    else
    {
        manifest->linkname = NULL;
    }

    manifest->has_dynamic_lib = false;
    manifest->has_static_lib = false;

    value = toml_table_get(lib_table, "type");

    if (value->type != TOML_ARRAY)
    {
        PRINT_ERROR("%s library.type was not an array", filename);
        return false;
    }

    TomlArray *array = value->value.array;
    for (unsigned i = 0; i < array->len; i++)
    {
        TomlValue *entry = array->elements[i];
        if (entry->type != TOML_STRING)
        {
            PRINT_ERROR("%s illegal library.type entry", filename);
            return false;
        }
        TomlString *str = entry->value.string;
        switch (component_type_from_toml(str))
        {
            case DYNAMIC:
                manifest->has_dynamic_lib = true;
                break;
            case STATIC:
                manifest->has_static_lib = true;
                break;
            case UNKNOWN:
                PRINT_ERROR("%s: invalid dep type '%.*s'", filename, (int) str->len, str->str);
                return false;
        }
    }
    return true;
}

static bool parse_module(TomlValue *value, const char *filename, Manifest *manifest)
{
    if (value->type != TOML_TABLE)
    {
        PRINT_ERROR("%s [module] was not a table", filename);
        return false;
    }
    TomlTable *deps_table = value->value.table;
    TomlValue *module_name = toml_table_get(deps_table, "name");
    if (!module_name || module_name->type != TOML_STRING)
    {
        PRINT_ERROR("%s: module needs name", filename);
        return false;
    }
    ManifestEntry *entry = malloc_arena(sizeof(ManifestEntry));
    entry->name = copy_string(module_name->value.string);
    entry->c2_file = NULL;
    vector_add(&manifest->entries, entry);
    return true;
}
/*
 *     TomlReader::NodeIter moduleIter = reader.getNodeIter("module");
     while (!moduleIter.done()) {
         const char* name = moduleIter.getValue("name");
         if (!name) {
             printf("%s: missing module name\n", filename.c_str());
             return false;
         }
         entries.push_back(ManifestEntry(name));
         moduleIter.next();
     }

 */

static bool parse_deps(TomlValue *value, const char *filename, Manifest *manifest)
{
    if (value->type != TOML_TABLE)
    {
        PRINT_ERROR("%s [deps] was not a table", filename);
        return false;
    }
    TomlTable *deps_table = value->value.table;
    TomlValue *dep_name = toml_table_get(deps_table, "name");
    TomlValue *dep_type = toml_table_get(deps_table, "type");

    if (!dep_name || !dep_type || dep_name->type != TOML_STRING || dep_type->type != TOML_STRING)
    {
        PRINT_ERROR("%s: dependency needs name and type", filename);
        return false;
    }
    TomlString *dep_type_str = dep_type->value.string;
    Dependency *dependency = malloc_arena(sizeof(Dependency));
    switch (component_type_from_toml(dep_type_str))
    {
        case DYNAMIC:
            dependency->type = COMPONENT_TYPE_SHARED_LIB;
            manifest->has_dynamic_lib = true;
            break;
        case STATIC:
            dependency->type = COMPONENT_TYPE_STATIC_LIB;
            manifest->has_static_lib = true;
            break;
        case UNKNOWN:
            PRINT_ERROR("%s: invalid dep type '%.*s'", filename, (int) dep_type_str->len, dep_type_str->str);
            return false;
    }
    TomlString *dep_name_str = dep_name->value.string;
    for (unsigned i = 0; manifest->deps.size; i++)
    {
        if (str_eq(dep_name_str, ((Dependency *) manifest->deps.entries[i])->name))
        {
            PRINT_ERROR("%s: Duplicate dependency on '%s'", filename, ((Dependency *) manifest->deps.entries[i])->name);
            return false;
        }
    }
    dependency->name = copy_string(dep_name_str);
    vector_add(&manifest->deps, dependency);
    return true;
}

bool manifest_parse(const char *filename, Manifest *manifest)
{
    vector_init(&manifest->entries, 8);
    vector_init(&manifest->deps, 8);
    bool ok = false;
    TomlErr err = TOML_ERR_INIT;
    TomlTable *table = toml_load_filename(filename, &err);
    if (err.code != TOML_OK)
    {
        PRINT_ERROR("%s: %s", filename, err.message);
        toml_clear_err(&err);
        return false;
    }

    TomlTableIter *it = toml_table_iter_new(table, &err);
    if (err.code != TOML_OK)
    {
        PRINT_ERROR("%s: %s", filename, err.message);
        toml_clear_err(&err);
        it = NULL;
        goto cleanup;
    }

    while (toml_table_iter_has_next(it))
    {
        TomlKeyValue *keyval = toml_table_iter_get(it);

        if (str_eq(keyval->key, "library"))
        {
            if (!parse_library(keyval->value, filename, manifest)) goto cleanup;
        }
        else if (str_eq(keyval->key, "deps"))
        {
            if (keyval->value->type == TOML_ARRAY)
            {
                TomlArray *array = keyval->value->value.array;
                for (unsigned i = 0; i < array->len; i++)
                {
                    if (!parse_deps(array->elements[i], filename, manifest)) goto cleanup;
                }
            }
            else
            {
                if (!parse_deps(keyval->value, filename, manifest)) goto cleanup;
            }
        }
        else if (str_eq(keyval->key, "module"))
        {
            if (keyval->value->type == TOML_ARRAY)
            {
                TomlArray *array = keyval->value->value.array;
                for (unsigned i = 0; i < array->len; i++)
                {
                    if (!parse_module(array->elements[i], filename, manifest)) goto cleanup;
                }
            }
            else
            {
                if (!parse_module(keyval->value, filename, manifest)) goto cleanup;
            }
        }
        else
        {
            PRINT_ERROR("%s, Unexpected element %.*s", filename, (int) keyval->key->len, keyval->key->str);
            goto cleanup;
        }
        toml_table_iter_next(it);
    }
    ok = true;
cleanup:
    if (it != NULL) toml_table_iter_free(it);
    toml_table_free(table);
    return ok;
}

