#pragma once

#include "common.h"
#include "array.h"
#include "component.h"
#include "target_info.h"

#define RECIPE_FILE "recipe.txt"
#define BUILD_FILE "build.toml"
#define MANIFEST_FILE "manifest"

typedef struct _BuildOptions
{
    bool print_ast_0;
    bool print_ast_1;
    bool print_ast_2;
    bool print_ast_3;
    bool print_ast_lib;
    bool print_timing;
    bool print_symbols;
    bool print_lib_symbols;
    bool generate_ir;
    bool print_ir;
    bool check_only;
    bool show_libs;
    bool print_modules;
    bool print_dependencies;
    bool generate_refs;
    bool verbose;
    bool test_mode;
    bool print_targets;
    bool skip_recipe;
    const char *target_filter;
    const char *build_file;
    const char *other_dir;
    const char *lib_dir;
} BuildOptions;


typedef struct _Recipe
{
    const char *name;
    ComponentType type;
    bool generate_deps;
    bool generate_refs;
    bool generate_ir;
    bool no_lib_c;

    Array files;
    Array configs;
    Array exported;
    Array c_configs;
    Array gen_configs;
    Array dep_configs;
    Array silent_warnings;
    Array libraries;
} Recipe;


Recipe *recipe_create(void);
void recipe_destroy(Recipe *recipe);
bool recipe_needs_interface(Recipe *recipe);
void recipe_add_file(Recipe *recipe, const char *name);
void recipe_add_config(Recipe *recipe, const char *mod);
void recipe_add_exported(Recipe *recipe, const char *mod);
void recipe_add_ansi_c_config(Recipe *recipe, const char *config);
void recipe_add_code_gen_config(Recipe *recipe, const char *config);
void recipe_add_dep_config(Recipe *recipe, const char *config);
void recipe_add_library(Recipe *recipe, const char *lib, ComponentType type);
bool recipe_has_library(Recipe *recipe, const char *lib);
void recipe_silence_warning(Recipe *recipe, const char *warning);
bool recipe_has_exported(Recipe *recipe, const char *mod);

typedef struct _BuildFile
{
    Array lib_dirs;
    const char *output_dir;
    const char *target;
} BuildFile;

typedef struct _Module Module;
typedef struct _Component Component;

typedef struct _LibInfo
{
    const char *header_lib_info;
    const char *c2_file;
    Component *component;
    Module *module;
    bool is_c_library;
} LibInfo;


int build_recipe(Recipe *recipe, BuildFile *build_file);


extern BuildOptions build_options;
