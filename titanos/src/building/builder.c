#include <string.h>
#include "builder.h"
#include <stdio.h>
#include "error.h"
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <dirent.h>
#include "diagnostics.h"
#include "parsing.h"
#include "arena_allocator.h"
#include "file.h"
#include "vector.h"
#include "table.h"
#include "module.h"
#include "manifest.h"
#include "dependecy.h"
#include "ast_types.h"
#include "semantic_analysis.h"
#include "codegen.h"

BuildOptions build_options = { 0 };

#define OUTPUT_DIR "output/"
#define BUILD_DIR  "build/"

Recipe *recipe_create(void)
{
    Recipe *r = malloc(sizeof(Recipe));
    r->generate_refs = false;
    r->generate_ir = false;
    r->no_lib_c = false;
    r->generate_deps = false;
    array_init(&r->gen_configs);
    array_init(&r->c_configs);
    array_init(&r->configs);
    array_init(&r->dep_configs);
    array_init(&r->exported);
    array_init(&r->files);
    array_init(&r->libraries);
    array_init(&r->silent_warnings);
    return r;
}

void recipe_destroy(Recipe *recipe)
{
    for (int i = 0; i < recipe->libraries.count; i++)
    {
        free(recipe->libraries.entries[i]);
    }
    array_free(&recipe->gen_configs);
    array_free(&recipe->c_configs);
    array_free(&recipe->configs);
    array_free(&recipe->dep_configs);
    array_free(&recipe->exported);
    array_free(&recipe->files);
    array_free(&recipe->libraries);
    array_free(&recipe->silent_warnings);
    array_free(&recipe->configs);
    array_free(&recipe->gen_configs);
    array_free(&recipe->c_configs);
    array_free(&recipe->configs);
}

bool recipe_needs_interface(Recipe *recipe)
{
    switch (recipe->type)
    {
        case COMPONENT_TYPE_EXECUTABLE:
            return false;
        case COMPONENT_TYPE_SHARED_LIB:
        case COMPONENT_TYPE_STATIC_LIB:
            return true;
        default:
            return false;
    }
}

void recipe_add_file(Recipe *recipe, const char *file)
{
    array_add(&recipe->files, (void *)file);
}

void recipe_add_config(Recipe *recipe, const char *config)
{
    array_add(&recipe->configs, (void *)config);
}

void recipe_add_exported(Recipe *recipe, const char *mod)
{
    array_add(&recipe->exported, (void *)mod);
}

void recipe_add_ansi_c_config(Recipe *recipe, const char *config)
{
    array_add(&recipe->c_configs, (void *)config);
}

void recipe_add_code_gen_config(Recipe *recipe, const char *config)
{
    array_add(&recipe->gen_configs, (void *)config);
}


void recipe_add_deps_config(Recipe *recipe, const char *config)
{
    array_add(&recipe->dep_configs, (void *)config);
}

void recipe_add_library(Recipe *recipe, const char *lib, ComponentType type)
{
    Dependency *dependency = malloc(sizeof(Dependency));
    dependency->type = type;
    dependency->name = lib;
    dependency->component = NULL;
    array_add(&recipe->libraries, dependency);
}

bool recipe_has_library(Recipe *recipe, const char *lib)
{
    for (unsigned i = 0; i < recipe->libraries.count; i++)
    {
        if (strcmp(lib, ((Dependency *)&recipe->libraries.entries[i])->name) == 0) return true;
    }
    return false;
}
void recipe_silence_warning(Recipe *recipe, const char *warning)
{
    array_add(&recipe->silent_warnings, (void *)warning);
}

bool recipe_has_exported(Recipe *recipe, const char *mod)
{
    return array_contains_string(&recipe->exported, mod);
}

typedef struct _LibraryLoader
{
    Vector lib_dirs;
    Table libraries;
    Table modules;
    Vector dependencies;
} LibraryLoader;

typedef struct _Builder
{
    Recipe *recipe;
    BuildFile *build_file;
    char *output_dir;
    Table modules;
    Module *c2_mod;
    Vector components;
    Component *main_component;

    TargetInfo target_info;
    //std::string outputDir;

    //ASTContext context;
    //Modules modules;
    //Module* c2Mod;

    //LibraryLoader libLoader;
    //bool useColors;
} Builder;

Builder builder;
LibraryLoader library_loader;

void build_teardown()
{
    free_arena();
    free(builder.output_dir); // Not arena allocated.
}

static void add_lib_dir(const char *dir)
{
    for (int i = 0; i < library_loader.lib_dirs.size; i++)
    {
        if (strcmp((const char *)(library_loader.lib_dirs.entries[i]), dir) == 0)
        {
            return;
        }
    }
    vector_add(&library_loader.lib_dirs, (void *)dir);
}

static void add_dependency(Component *component, const char *dest, ComponentType type)
{
    Dependency *dependecy = malloc(sizeof(Dependency));
    dependecy->component = component;
    dependecy->type = type;
    dependecy->name = dest;
    vector_add(&library_loader.dependencies, dependecy);
}

static void build_setup()
{
    init_arena();
    builder.c2_mod = NULL;
    builder.main_component = NULL;
    table_init(&builder.modules, 8);
    vector_init(&builder.components, 8);
    vector_init(&library_loader.lib_dirs, 8);
    table_init(&library_loader.libraries, 32);
    table_init(&library_loader.modules, 32);
    vector_init(&library_loader.dependencies, 8);

    if (builder.build_file && builder.build_file->output_dir)
    {
        int res = asprintf(&builder.output_dir, "%s/%s/", builder.build_file->output_dir, builder.recipe->name);
        ASSERT(res != -1, "Could not join string");
    }
    else
    {
        int res = asprintf(&builder.output_dir, OUTPUT_DIR "%s/", builder.recipe->name);
        ASSERT(res != -1, "Could not join string");
    }

    if (builder.build_file)
    {
        for (unsigned i = 0; i < builder.build_file->lib_dirs.count; i++)
        {
            add_lib_dir(builder.build_file->lib_dirs.entries[i]);
        }
        if (!builder.build_file->target)
        {
            target_info_from_native(&builder.target_info);
        }
        else
        {
            if (!target_info_from_string(&builder.target_info, builder.build_file->target))
            {
                PRINT_ERROR("error: invalid target string '%s'", builder.build_file->target);
                exit(EXIT_FAILURE);
            }
        }
    }
    else
    {
        if (build_options.lib_dir)
        {
            add_lib_dir(build_options.lib_dir);
        }
        target_info_from_native(&builder.target_info);
    }
    if (build_options.verbose)
    {
        LOG(COL_VERBOSE, "Target: %s", target_info_to_string(&builder.target_info));
    }

}
static int check_files(Recipe *recipe)
{
    int errors = 0;
    for (unsigned i = 0; i < recipe->files.count; i++)
    {
        const char *filename = recipe->files.entries[i];
        struct stat buf;
        if (stat(filename, &buf))
        {
            PRINT_ERROR("error: %s '%s'", strerror(errno), filename);
            errors++;
        }
    }
    return errors;
}

static int make_build(void);

int build_recipe(Recipe *recipe, BuildFile *build_file)
{
    builder.build_file = build_file;
    builder.recipe = recipe;
    build_setup();
    int errors = check_files(recipe);
    if (!errors)
    {
        errors = make_build();
    }
    build_teardown();
    return errors;
}

static int component_compare(const void *c1, const void *c2)
{
    if (component_has_dependency(*((Component **)c1), *((Component **)c2))) return -1;
    if (component_has_dependency(*((Component **)c2), *((Component **)c1))) return 1;
    return 0;
}

static bool find_component_dir(const char *name, char *dir_name)
{
    for (unsigned i = 0; i < library_loader.lib_dirs.size; i++)
    {
        char *lib_dir = library_loader.lib_dirs.entries[i];
        char full_name[512];
        snprintf(full_name, 512, "%s/%s/" MANIFEST_FILE, lib_dir, name);
        struct stat statbuf;
        if (stat(full_name, &statbuf) == 0)
        {
            snprintf(dir_name, 512, "%s/%s", lib_dir, name);
            return true;
        }
    }
    dir_name[0] = 0;
    return false;
}

static void add_lib_info(const char *c2_file, bool c_lib, Component *component, Module *module, const char *header_lib_info)
{
    LibInfo *info = malloc_arena(sizeof(LibInfo));
    info->c2_file = c2_file;
    info->is_c_library = c_lib;
    info->component = component;
    info->module = module;
    info->header_lib_info = header_lib_info;
    table_set(&library_loader.libraries, module->name.start, module->name.length, info);

}
static Component *find_component_with_module_named(Token *module_name)
{
    for (unsigned i = 0; i < builder.components.size; i++)
    {
        Component *c = builder.components.entries[i];
        if (component_find_module(c, module_name)) return c;
    }
    return NULL;
}

static bool check_library(Dependency *dep)
{
    bool has_errors = false;
    const char *component_name = dep->name;

    for (unsigned i = 0; i < builder.components.size; i++)
    {
        Component *c = builder.components.entries[i];
        if (strcmp(c->name, component_name) == 0)
        {
            vector_add(&dep->component->dependencies, c);
            return false;
        }
    }

    // TODO search all libDirs until file is found
    char component_dir[512];
    if (!find_component_dir(component_name, component_dir))
    {
        PRINT_ERROR("error: cannot find library '%s'", component_name);
        return true;
    }
    char fullname[512];
    snprintf(fullname, 512, "%s/%s", component_dir, MANIFEST_FILE);

    Manifest manifest;
    if (!manifest_parse(fullname, &manifest)) return true;
    if (dep->type == COMPONENT_TYPE_SHARED_LIB && !manifest.has_dynamic_lib)
    {
        PRINT_ERROR("'%s' is not available as shared library in %s", component_name, fullname);
        has_errors = true;
    }
    if (dep->type == COMPONENT_TYPE_STATIC_LIB && !manifest.has_static_lib)
    {
        PRINT_ERROR("'%s' is not available as static library in %s", component_name, fullname);
        has_errors = true;
    }

    Component *component = component_create(component_name, component_dir, dep->type, true,
            !manifest.is_native, &builder.recipe->exported);

    vector_add(&builder.components, component);
    vector_add(&dep->component->dependencies, component);
    component->link_name = manifest.linkname;

    for (unsigned i = 0; i < manifest.deps.size; i++)
    {
        add_dependency(component, manifest.deps.entries[i], dep->type);
    }

    for (unsigned i = 0; i < manifest.entries.size; i++)
    {
        ManifestEntry *entry = manifest.entries.entries[i];
		Token name = token_wrap(entry->name);
        Module *module = component_get_module(component, &name);
        if (table_get(&library_loader.modules, name.start, name.length))
        {
            Component *other = find_component_with_module_named(&name);
            assert(other);
            PRINT_ERROR("Module name '%s' is used in %s and %s",
                    entry->name, component->name, other->name);
            has_errors = true;
        }
        else
        {
            table_set(&library_loader.modules, name.start, name.length, module);
        }
        char *c2file = malloc_arena(512);
        snprintf(c2file, 512, "%s/%s.c2i", component_dir, entry->name);
        char *header_file = malloc_arena(name.length + 4);
        snprintf(header_file, name.length + 4, "%s.h", entry->name);
        add_lib_info(c2file, !manifest.is_native, component, module, header_file);
    }
    return has_errors;
}

static int scan_libraries()
{
    assert(builder.components.size == 1 && "Only expected main component");
    Component *component = builder.components.entries[0];
    // create libs entry for MainComponent
    for (int i = 0; i < component->modules.size; i++)
    {
        Module *module = component->modules.entries[i];
        table_set(&library_loader.modules, module->name.start, module->name.length, module);
        add_lib_info("", true, component, module, "");
    }
    // Dependencies for main component have already been added

    for (unsigned j = 0; j < library_loader.lib_dirs.size; j++)
    {
        const char *lib_dir = library_loader.lib_dirs.entries[j];
        struct stat statbuf;
        int err = stat(lib_dir, &statbuf);
        if (err)
        {
            PRINT_ERROR("error: cannot open library dir %s: %s\n", lib_dir, strerror(errno));
            // just continue to get 'cannot find library X' errors
        }
    }

    bool has_errors = false;
    while (library_loader.dependencies.size)
    {
        Dependency *dep = library_loader.dependencies.entries[--library_loader.dependencies.size];
        has_errors |= check_library(dep);
    }
    if (!has_errors)
    {
        qsort(builder.components.entries, builder.components.size, sizeof(Component *), &component_compare);
    }
    return !has_errors;
}

static void show_lib(const char *lib_dir)
{
    printf("  ");
    LOG(ANSI_BLUE, "%s", lib_dir);
    DIR *dir = opendir(lib_dir);
    if (!dir)
    {
        PRINT_ERROR("error: cannot open library dir '%s'", lib_dir);
        return;
    }
    struct dirent *entry = readdir(dir);
    while (entry)
    {
        if (entry->d_name[0] == '.') goto next;
        if (entry->d_type == DT_DIR)
        {
            // check for manifest file
            char fullname[512];
            snprintf(fullname, 512, "%s/%s/%s", lib_dir, entry->d_name, MANIFEST_FILE);
            struct stat statbuf;
            int err = stat(fullname, &statbuf);
            if (err) goto next;
            printf("    %s", entry->d_name);
            printf("\n"); // REMOVE
#ifdef TODO
            {
                ManifestReader manifest(fullname);
                if (!manifest.parse()) {
                    fprintf(stderr, "c2c: error: %s: %s\n", fullname, manifest.getErrorMsg());
                    goto next;
                }
                out << "  ";
                out.setColor(ANSI_YELLOW);
                if (manifest.hasStatic()) out << "static";
                if (manifest.hasDynamic()) {
                    if (manifest.hasStatic()) out << '|';
                    out << "dynamic";
                }
                out << "  ";
                const StringList& deps = manifest.getDeps();
                if (!deps.empty()) {
                    out.setColor(ANSI_MAGENTA);
                    out << "requires: ";
                    for (unsigned i=0; i<deps.size(); i++) {
                        if (i != 0) out << ", ";
                        out << deps[i];
                    }
                }
                out.setColor(ANSI_NORMAL);
                out << '\n';

                for (unsigned e=0; e<manifest.numEntries(); e++) {
                    const ManifestEntry& entry2 = manifest.get(e);
                    out << "      " << entry2.name << '\n';
                }
            }
#endif
        }
next:
        entry = readdir(dir);
    }
}

static void show_libs(void)
{
    LOG(ANSI_NORMAL, "libraries");
    for (unsigned i = 0; library_loader.lib_dirs.size; i++)
    {
        show_lib(library_loader.lib_dirs.entries[i]);
    }
}

static LibInfo *find_module_lib(Token *module_name)
{
    return table_get(&library_loader.libraries, module_name->start, module_name->length);
}

static bool parse_file(Component *component, Module *module, const char *filename, bool show_ast, bool is_interface)
{
    Parser *parser = parse(filename, is_interface);
    if (!parser) return false;
    if (show_ast)
    {
        printf("[IMPORTS]\n");
        for (unsigned i = 0; i < parser->imports->size; i++)
        {
            decl_print(parser->imports->entries[i], 1);
        }
        printf("[FUNCTIONS]\n");
        for (unsigned i = 0; i < parser->functions->size; i++)
        {
            decl_print(parser->functions->entries[i], 1);
        }
        printf("[TYPES]\n");
        for (unsigned i = 0; i < parser->types->size; i++)
        {
            decl_print(parser->types->entries[i], 1);
        }
        printf("[VARIABLES]\n");
        for (unsigned i = 0; i < parser->variables->size; i++)
        {
            decl_print(parser->variables->entries[i], 1);
        }
    }
    if (module)
    {
        // for external modules, filename should match module name
        if (!token_compare(&module->name, &parser->current_module))
        {
            error_at(&parser->current_module, "Module does not match filename '%.*s'",
                     parser->current_module.length, parser->current_module.start,
                     module->name.length, module->name.start);
            return false;
        }
    }
    else
    {
        module = component_get_module(component, &parser->current_module);
    }
    vector_add(module->files, parser);
    return true;
}

static Module *c2_module;


static void register_c2_constant(const char *name, const char *type, Value value)
{
    Token token = token_wrap(name);
    Token type_token = token_wrap(type);
    Decl *decl = decl_new(DECL_VAR, &token, &token, true);
    Ast *ast_value = new_ast_with_span(AST_CONST_EXPR, &token);
    ast_value->const_expr.value = value;
    Ast *type_ast = new_type_expr(TYPE_EXPR_IDENTIFIER, &type_token);
    type_ast->type_expr.identifier_type_expr.name = token;
    type_ast->type_expr.identifier_type_expr.module_name.length = 0;
    type_ast->type_expr.identifier_type_expr.resolved_type = NULL;
    decl->var.init_expr = ast_value;
    decl->var.kind = VARDECL_GLOBAL;
    decl->is_exported = true;
    decl->type = new_type(TYPE_UNRESOLVED, true, &type_token);
    decl->type->unresolved.type_expr = type_ast;
    Parser *parser = c2_module->files->entries[0];
    vector_add(parser->variables, decl);
}
static void register_c2_unsigned_constant(const char *type, const char *name, uint64_t size)
{
    register_c2_constant(name, type, value_new_int_with_int(size));
}
static void register_c2_signed_constant(const char *type, const char *name, int64_t size)
{
    register_c2_constant(name, type, value_new_int_with_int(size));
}

static void register_c2_builtin(const char *name, TypeId family, unsigned bits, bool is_signed)
{
    Token token = token_wrap(name);
    Decl *alias = decl_new(DECL_ALIAS_TYPE, &token, &token, true);
    Type *builtin = new_type(family, true, &token);
    switch (family)
    {
        case TYPE_INT:
            builtin->integer.bits = bits;
            builtin->integer.is_signed = is_signed;
            break;
        case TYPE_FLOAT:
            builtin->real.bits = bits;
            break;
        case TYPE_BOOL:
            break;
        default:
            FATAL_ERROR("Unsupported builtin");
    }
    alias->module = c2_module;
    alias->type = builtin;
    builtin->module = c2_module;
    module_add_symbol(c2_module, &token, alias);
}

static void create_c2_module()
{
    if (!c2_module)
    {
        if (build_options.verbose) LOG(COL_VERBOSE, "generating module c2");
        c2_module = malloc(sizeof(Module));
        Token token = token_wrap("c2");
        module_init(c2_module, &token, true, false);
        table_set_token(&library_loader.modules, &c2_module->name, c2_module);
        Parser *parser = malloc_arena(sizeof(Parser));
        init_parser(parser, "c2", true);
        vector_add(c2_module->files, parser);

        init_parser(parser, "c2", true);
        parser->current_module = token_wrap("c2");
        register_c2_signed_constant("i64", "buildtime", time(0));
        register_c2_signed_constant("i8", "min_i8", -0x80);
        register_c2_signed_constant("i8", "max_i8", 0x7F);
        register_c2_unsigned_constant("u8", "min_u8", 0);
        register_c2_unsigned_constant("u8", "max_u8", 0xFF);
        register_c2_signed_constant("i16", "min_i16", -0x8000);
        register_c2_signed_constant("i16", "max_i16", 0x7FFF);
        register_c2_unsigned_constant("u16", "min_u16", 0);
        register_c2_unsigned_constant("u16", "max_u16", 0xFFFF);
        register_c2_signed_constant("i32", "min_i32", -0x80000000);
        register_c2_signed_constant("i32", "max_i32", 0x7FFFFFFF);
        register_c2_unsigned_constant("u32", "min_u32", 0);
        register_c2_unsigned_constant("u32", "max_u32", 0xFFFFFFFFFFFFFFFFllu);
        register_c2_signed_constant("i64", "min_i64", -0x800000000000000ll);
        register_c2_signed_constant("i64", "max_i64", 0x7FFFFFFFFFFFFFFFll);
        register_c2_unsigned_constant("u64", "min_u64", 0);
        register_c2_unsigned_constant("u64", "max_u64", 0xFFFFFFFFFFFFFFFFllu);
        register_c2_builtin("u8", TYPE_INT, 8, false);
        register_c2_builtin("u16", TYPE_INT, 16, false);
        register_c2_builtin("u32", TYPE_INT, 32, false);
        register_c2_builtin("u64", TYPE_INT, 64, false);
        register_c2_builtin("i8", TYPE_INT, 8, true);
        register_c2_builtin("i16", TYPE_INT, 16, true);
        register_c2_builtin("i32", TYPE_INT, 32, true);
        register_c2_builtin("i64", TYPE_INT, 64, true);
        register_c2_builtin("f32", TYPE_FLOAT, 32, true);
        register_c2_builtin("f64", TYPE_FLOAT, 64, true);
        register_c2_builtin("bool", TYPE_BOOL, 1, true);
    }
};

static bool check_module_imports_and_parse(Component *component, Module *module, Vector *queue, const LibInfo *lib)
{
    if (module->files->size == 0)
    {
        assert(lib);
        if (build_options.verbose)
        {
            LOG(COL_VERBOSE, "parsing (%s) %s", component->name, lib->c2_file);
        }
        if (!parse_file(component, module, lib->c2_file, build_options.print_ast_0 && build_options.print_ast_lib, true)) return false;
    }
    if (build_options.verbose) LOG(COL_VERBOSE, "checking imports for module (%s) %.*s", component->name, (int)module->name.length, module->name.start);
    bool ok = true;
    Vector *files = module->files;
    for (unsigned a = 0; a < files->size; a++)
    {
        Parser* parser = files->entries[a];
        // NOTE: first import is module
        Decl *module_import = parser->imports->entries[0];
        module_import->module = module;
        for (unsigned u = 1; u < parser->imports->size; u++)
        {
            Decl *import = parser->imports->entries[u];
            assert(import);
            if (exceeds_identifier_len(&import->name))
            {
                error_at(&import->name, "Module name exceeds max length");
                ok = false;
                continue;
            }
            // handle c2 pseudo-module
            if (token_compare_str(&import->name, "c2"))
            {
                create_c2_module();
                import->module = c2_module;
                continue;
            }
            const LibInfo *target = find_module_lib(&import->name);
            if (!target)
            {
                error_at(&import->name, "Unknown module");
                ok = false;
                continue;
            }
            import->module = target->module;
            if (target->component != component)
            {
                // check that imports are in directly dependent component (no indirect component)
                if (!component_has_dependency(component, target->component))
                {
                    error_at(&import->span, "%s has no dependency on library %s, needed for module %.*s",
                            component->name, target->component->name,
                            SPLAT_TOK(import->name));
                    ok = false;
                    continue;
                }
            }
            if (target->module->files->size) continue;
            vector_add(queue, &import->name);
        }
    }
    return ok;
}

static bool check_main_function()
{
    Ast *main_decl = NULL;
    Token main_name = token_wrap("main");
    for (unsigned i = 0; i < builder.main_component->modules.size; i++)
    {
        Module *module = builder.main_component->modules.entries[i];
        Ast *main = module_find_symbol(module, &main_name);
        if (!main) continue;
        if (main_decl)
        {
            sema_error_at(&main->span, "Multiple main function");
            sema_error_at(&main_decl->span, "Previous one was here");
        }
        else
        {
            main_decl = main;
        }
    }

    if (builder.recipe->type == COMPONENT_TYPE_EXECUTABLE)
    {
        if (build_options.test_mode) return true;
        if (!main_decl)
        {
            PRINT_ERROR("Missing main function");
            return false;
        }
    }
    else if (main_decl)
    {
        sema_error_at(&main_decl->span, "Libraries cannot have main functions");
        return false;
    }
    return true;
}

static void generate_optional_ir()
{
    bool single_module = true;
    if (single_module)
    {
        clock_t t1 = clock();
        const char *name = builder.recipe->name;
        if (build_options.verbose)
        {
            LOG(COL_VERBOSE, "Generating IR for single module %s", name);
        }
        codegen(name, true, &builder.main_component->modules);
        if (build_options.print_timing)
        {
            LOG(COL_TIME, "IR generation took %ld ms", (clock() - t1) * 1000 / CLOCKS_PER_SEC);
        }
        // TODO put it in the codegen.
/*        build_options.
        if (options.printIR) cgm.dump();
        bool ok = cgm.verify();
        if (ok) cgm.write(buildDir, filename);*/
    }
    else
    {

        /*
         *         for (unsigned m=0; m<mods.size(); m++) {
                     Module* M = mods[m];
                     uint64_t t1 = Utils::getCurrentTime();
                     if (M->isPlainC()) continue;
                     if (M->getName() == "c2") continue;

                     if (options.verbose) log(COL_VERBOSE, "generating IR for module %s", M->getName().c_str());
                     ModuleList single;
                     single.push_back(M);
                     CodeGenModule cgm(M->getName(), false, single, context);
                     cgm.generate();
                     uint64_t t2 = Utils::getCurrentTime();
                     if (options.printTiming) log(COL_TIME, "IR generation took %" PRIu64" usec", t2 - t1);
                     if (options.printIR) cgm.dump();
                     bool ok = cgm.verify();
                     if (ok) cgm.write(buildDir, M->getName());
                 }

         */
    }
}

static void generate_interface()
{}

static void generate_optional_deps()
{}


static Module *find_module(const char *name)
{
    return table_get_string(&builder.modules, name);
}

static bool check_exported_packages()
{
    for (unsigned i = 0; i < builder.recipe->exported.count; i++)
    {
        const char *name = builder.recipe->exported.entries[i];
        Module *module = find_module(name);
        if (!module)
        {
            PRINT_ERROR("cannot export '%s', no such module\n", name);
            return false;
        }
        if (module->is_external)
        {
            PRINT_ERROR("cannot export external module '%s'\n", name);
            return false;
        }
    }
    return true;
}


static bool check_imports()
{
    Vector imports_queue;
    vector_init(&imports_queue, 8);
    Vector *main_modules = &builder.main_component->modules;
    bool ok = true;
    for (unsigned i = 0; i < main_modules->size; i++)
    {
        ok &= check_module_imports_and_parse(builder.main_component, main_modules->entries[i], &imports_queue, NULL);
    }
    if (!ok) return false;
    for (unsigned i = 0; i < imports_queue.size; i++)
    {
        Token *current_module = imports_queue.entries[i];
        LibInfo *lib_info = find_module_lib(current_module);
        assert(lib_info && "Missing library info??");
        ok &= check_module_imports_and_parse(lib_info->component, lib_info->module, &imports_queue, lib_info);
    }
    return ok;
}

static void print_components()
{
    if (c2_module)
    {
        printf("Component <internal>\n");
        module_print_files(c2_module);
    }

    for (unsigned i = 0; i < builder.components.size; i++)
    {
        component_print(builder.components.entries[i]);
    }
}

static void print_symbols(bool print_libs)
{
    assert(builder.main_component);
    if (print_libs)
    {
        if (c2_module)
        {
            printf("Component <internal>\n");
            module_print_symbols(c2_module);
        }
        for (unsigned i = 0; i < builder.components.size; i++)
        {
            component_print_symbols(builder.components.entries[i]);
        }
    }
    else
    {
        component_print_symbols(builder.main_component);
    }
}
static int make_build(void)
{
    LOG(ANSI_GREEN, "building target %s", builder.recipe->name);

    clock_t build_time = clock();
    diagnostics_init();
    diagnostics_use_color(!build_options.test_mode && isatty(2));

    // set recipe warning options
    bool success = diagnostics_silence_warnings(&builder.recipe->silent_warnings);
    if (!success) exit(EXIT_FAILURE);

#if 0
    // TargetInfo
    std::shared_ptr<TargetOptions> to(new TargetOptions());
    to->Triple = llvm::sys::getDefaultTargetTriple();

    std::shared_ptr<HeaderSearchOptions> HSOpts(new HeaderSearchOptions());
    // add current directory (=project root) to #include path
    char pwd[512];
    if (getcwd(pwd, 512) == 0) {
        FATAL_ERROR("Failed to get current directory");
    }
    HSOpts->AddPath(pwd, c2lang::frontend::Quoted);

    // set definitions from recipe
    std::string PredefineBuffer;
    if (!recipe.configs.empty()) {
        PredefineBuffer.reserve(4080);
        llvm::raw_string_ostream Predefines(PredefineBuffer);
        MacroBuilder mbuilder(Predefines);
        for (unsigned i=0; i<recipe.configs.size(); i++) {
            mbuilder.defineMacro(recipe.configs[i]);
        }
    }

    // FileManager
    FileSystemOptions FileSystemOpts;
    FileManager FileMgr(FileSystemOpts);
    SourceManager SM(Diags, FileMgr);
    MemoryBufferCache PCMCache;
#endif

    // create main Component
    // Maybe use arena malloc? TODO
    builder.main_component = component_create(builder.recipe->name, "TODO", builder.recipe->type, false, false, &builder.recipe->exported);

    vector_add(&builder.components, builder.main_component);

    // NOTE: libc always SHARED_LIB for now
    if (!builder.recipe->no_lib_c)
    {
        add_dependency(builder.main_component, "libc", COMPONENT_TYPE_SHARED_LIB);
    }

    for (unsigned i = 0; i < builder.recipe->libraries.count; i++)
    {
        Dependency *lib = builder.recipe->libraries.entries[i];
        add_dependency(builder.main_component, lib->name, lib->type);
    }

    clock_t parse_time = clock();

    unsigned errors = 0;
    // phase 1a: parse and local analyse
    for (unsigned i = 0; i < builder.recipe->files.count; i++)
    {
        const char *filename = builder.recipe->files.entries[i];

        if (build_options.verbose)
        {
            LOG(COL_VERBOSE, "parsing (%s) %s", builder.main_component->name, filename);
        }
        if (!parse_file(builder.main_component, NULL, filename, build_options.print_ast_0, false))
        {
            errors++;
        }
    }

    if (build_options.print_timing)
    {
        LOG(COL_TIME, "parsing took %ld ms", (clock() - parse_time) * 1000 / CLOCKS_PER_SEC);
    }

    if (errors) goto out;
    // phase 1b: load required external Components/Modules
    {
        clock_t parse_lib_time = clock();
        if (!scan_libraries()) goto out;
        if (build_options.show_libs) show_libs();
        bool ok = check_imports();
        if (build_options.print_timing)
        {
            LOG(COL_TIME, "parsing libraries took %ld ms", (clock() - parse_lib_time) * 1000 / CLOCKS_PER_SEC);
        }
        if (!ok) goto out;
    }

    // phase 2: analyse all files
    {
        clock_t start_time = clock();
        for (unsigned c = 0; c < builder.components.size; c++)
        {
            Component *component = builder.components.entries[c];
            if (build_options.verbose)
            {
                LOG(COL_VERBOSE, "analysing component %s", component->name);
            }
            errors += analyse(component, &library_loader.modules);
        }
        if (build_options.print_timing)
        {
            LOG(COL_TIME, "analysis took %ld ms", (clock() - start_time) * 1000 / CLOCKS_PER_SEC);
        }
    }

    if (build_options.print_modules)
    {
        print_components();
    }
    if (build_options.print_symbols)
    {
        print_symbols(build_options.print_lib_symbols);
    }
    if (errors) goto out;

    if (!check_main_function()) goto out;

    if (!check_exported_packages()) goto out;

    generate_optional_deps();

    // generate_optional_tags();

    generate_interface();

    generate_optional_ir();

    if (build_options.verbose)
    {
        LOG(COL_VERBOSE, "done");
    }

    out:

    if (build_options.print_timing)
    {
        LOG(COL_TIME, "Total build took %ld ms", (clock() - build_time) * 1000 / CLOCKS_PER_SEC);
    }
    /*
    unsigned NumWarnings = client->getNumWarnings();
    unsigned NumErrors = client->getNumErrors();
    if (NumWarnings)
        OS << NumWarnings << " warning" << (NumWarnings == 1 ? "" : "s");
    if (NumWarnings && NumErrors)
        OS << " and ";
    if (NumErrors)
        OS << NumErrors << " error" << (NumErrors == 1 ? "" : "s");
    if (NumWarnings || NumErrors)
        OS << " generated.\n";
    return NumErrors;
*/
    return 0;
}

#if 0





void C2Builder::generateOptionalDeps() {
    if (!options.printDependencies && !recipe.generateDeps) return;

    if (options.verbose) log(COL_VERBOSE, "generating dependencies");

    uint64_t t1 = Utils::getCurrentTime();
    bool showFiles = false;
    bool showExternals = false;
    bool showPrivate = true;
    for (unsigned i=0; i<recipe.depConfigs.size(); i++) {
        const std::string& conf = recipe.depConfigs[i];
        if (conf == "show-files") showFiles = true;
        if (conf == "show-externals") showExternals = true;
    }

    generateDeps(showFiles, showPrivate, showExternals, outputDir);
    uint64_t t2 = Utils::getCurrentTime();
    if (options.printTiming) log(COL_TIME, "dep generation took %" PRIu64" usec", t2 - t1);
}

void C2Builder::generateDeps(bool showFiles, bool showPrivate, bool showExternals, const std::string& path) const {
    DepGenerator generator(showFiles, showPrivate, showExternals);
    generator.write(components, recipe.name, path);
}

void C2Builder::generateOptionalTags(const SourceManager& SM) const {
    if (!options.generateRefs && !recipe.generateRefs) return;

    if (options.verbose) log(COL_VERBOSE, "generating refs");

    uint64_t t1 = Utils::getCurrentTime();
    TagWriter generator(SM, components);
    generator.write(recipe.name, outputDir);
    uint64_t t2 = Utils::getCurrentTime();
    if (options.printTiming) log(COL_TIME, "refs generation took %" PRIu64" usec", t2 - t1);
}

void C2Builder::generateInterface() const {
    if (options.checkOnly) return;
    if (!options.generateC && !recipe.generateCCode &&
            !options.generateIR && !recipe.generateIR) return;
    if (!recipe.needsInterface()) return;

    if (options.verbose) log(COL_VERBOSE, "generating c2 interfaces");

    const ModuleList& mods = mainComponent->getModules();
    for (unsigned m=0; m<mods.size(); m++) {
        const Module* M = mods[m];
        if (!M->isExported()) continue;
        InterfaceGenerator gen(*M);
        gen.write(outputDir, options.printC);
    }

    ManifestWriter manifest(*mainComponent);
    manifest.write(outputDir);
}

void C2Builder::generateOptionalC() {
    if (options.checkOnly) return;
    if (!options.generateC && !recipe.generateCCode) return;

    uint64_t t1 = Utils::getCurrentTime();
    bool single_module = false;
    bool no_build = false;
    for (unsigned i=0; i<recipe.cConfigs.size(); i++) {
        const std::string& conf = recipe.cConfigs[i];
        // TODO just pass struct with bools?
        if (conf == "single-module") single_module = true;
        else if (conf == "no-build") no_build = true;
        else {
            fprintf(stderr, ANSI_RED"invalid c-generation argument '%s'" ANSI_NORMAL"\n", conf.c_str());
        }
    }

    CGenerator::Options cgen_options(outputDir, BUILD_DIR);
    cgen_options.single_module = single_module;
    cgen_options.printC = options.printC;
    CGenerator cgen(*mainComponent, modules, libLoader, cgen_options, targetInfo, buildFile);

    // generate headers for external libraries
    if (options.verbose) log(COL_VERBOSE, "generating external headers");
    cgen.generateExternalHeaders();

    // generate C interface files
    if (recipe.needsInterface()) {
        if (options.verbose) log(COL_VERBOSE, "generating interface headers");
        cgen.generateInterfaceFiles();
    }

    // use C-backend
    if (options.verbose) log(COL_VERBOSE, "generating C code");
    cgen.generate();

    uint64_t t2 = Utils::getCurrentTime();
    if (options.printTiming) log(COL_TIME, "C code generation took %" PRIu64" usec", t2 - t1);

    if (!no_build) {
        if (options.verbose) log(COL_VERBOSE, "building C code");
        uint64_t t3 = Utils::getCurrentTime();
        cgen.build();
        uint64_t t4 = Utils::getCurrentTime();
        if (options.printTiming) log(COL_TIME, "C code compilation took %" PRIu64" usec", t4 - t3);
    }
}

void C2Builder::generateOptionalIR() {
    if (options.checkOnly) return;
    if (!options.generateIR && !recipe.generateIR) return;

    bool single_module = false;
    for (unsigned i=0; i<recipe.genConfigs.size(); i++) {
        const std::string& conf = recipe.genConfigs[i];
        // TODO just pass struct with bools?
        if (conf == "single-module") single_module = true;
        else {
            fprintf(stderr, ANSI_RED"invalid code generation argument '%s'" ANSI_NORMAL"\n", conf.c_str());
        }
    }

    std::string buildDir = outputDir + BUILD_DIR;

    // TODO move all this to some generic Codegen class
    // Q: use single context or one-per-module?
    llvm::LLVMContext context;

    const ModuleList& mods = mainComponent->getModules();
    if (single_module) {
        uint64_t t1 = Utils::getCurrentTime();
        std::string filename = recipe.name;
        if (options.verbose) log(COL_VERBOSE, "generating IR for single module %s", filename.c_str());
        CodeGenModule cgm(filename, true, mods, context);
        cgm.generate();
        uint64_t t2 = Utils::getCurrentTime();
        if (options.printTiming) log(COL_TIME, "IR generation took %" PRIu64" usec", t2 - t1);
        if (options.printIR) cgm.dump();
        bool ok = cgm.verify();
        if (ok) cgm.write(buildDir, filename);
    } else {
        for (unsigned m=0; m<mods.size(); m++) {
            Module* M = mods[m];
            uint64_t t1 = Utils::getCurrentTime();
            if (M->isPlainC()) continue;
            if (M->getName() == "c2") continue;

            if (options.verbose) log(COL_VERBOSE, "generating IR for module %s", M->getName().c_str());
            ModuleList single;
            single.push_back(M);
            CodeGenModule cgm(M->getName(), false, single, context);
            cgm.generate();
            uint64_t t2 = Utils::getCurrentTime();
            if (options.printTiming) log(COL_TIME, "IR generation took %" PRIu64" usec", t2 - t1);
            if (options.printIR) cgm.dump();
            bool ok = cgm.verify();
            if (ok) cgm.write(buildDir, M->getName());
        }
    }
}


#endif
