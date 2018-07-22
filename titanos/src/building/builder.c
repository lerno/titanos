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

BuildOptions build_options = { 0 };

#define OUTPUT_DIR "output/"
#define BUILD_DIR  "build/"

Recipe *recipe_create(void)
{
    Recipe *r = malloc(sizeof(Recipe));
    r->generate_refs = false;
    r->generate_ir = false;
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
    Vector modules;
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
    vector_init(&builder.modules, 8);
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
    if (component_has_dependency(c1, c2)) return -1;
    if (component_has_dependency(c2, c1)) return 1;
    return 0;
}

static bool find_component_dir(const char *name, char *dir_name)
{
    for (unsigned i = 0; i < library_loader.lib_dirs.size; i++)
    {
        char *lib_dir = library_loader.lib_dirs.entries[i];
        char full_name[512];
        sprintf(full_name, "%s/%s/%s", lib_dir, name, MANIFEST_FILE);
        struct stat statbuf;
        if (stat(full_name, &statbuf) == 0)
        {
            sprintf(dir_name, "%s/%s", lib_dir, name);
            return true;
        }
    }
    dir_name[0] = 0;
    return false;
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
            array_add(&dep->component->dependencies, c);
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
    sprintf(fullname, "%s/%s", component_dir, MANIFEST_FILE);

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
    array_add(&dep->component->dependencies, component);
    component->link_name = manifest.linkname;

    for (unsigned i = 0; i < manifest.deps.size; i++)
    {
        add_dependency(component, manifest.deps.entries[i], dep->type);
    }

    for (unsigned i = 0; i < manifest.entries.size; i++)
    {
        ManifestEntry *entry = manifest.entries.entries[i];
#ifdef TODO
        const std::string& moduleName = entry.name;
        Module* M = C->getModule(moduleName);

        ModulesConstIter iter = modules.find(moduleName);
        if (iter != modules.end()) {
            const Component* other = findModuleComponent(moduleName);
            assert(other);
            fprintf(stderr, "c2c: error: module-name '%s' is used in %s and %s\n",
                moduleName.c_str(), C->getName().c_str(), other->getName().c_str());
            hasErrors = true;
        } else {
            modules[moduleName] = M;
        }

        StringBuilder c2file(512);
        c2file << componentDir << '/' << moduleName << ".c2i";

        libs[moduleName] = new LibInfo(moduleName + ".h", c2file.c_str(), C, M, !manifest.isNative());
#endif
    }
    return has_errors;
}

static int scan_libraries()
{
    assert(builder.components.size == 1 && "Only expected main component");
    Component *component = builder.components.entries[0];
    // create libs entry for MainComponent
    for (int i = 0; i < component->modules.count; i++)
    {
        Module *module = component->modules.entries[i];
        table_set(&library_loader.modules, module->name.start, module->name.length, module);

        LibInfo *info = malloc_arena(sizeof(LibInfo));
        info->c2_file = "";
        info->is_c_library = true;
        info->component = component;
        info->module = module;
        info->header_lib_info = "";
        table_set(&library_loader.libraries, module->name.start, module->name.length, info);
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
            sprintf(fullname, "%s/%s/%s", lib_dir, entry->d_name, MANIFEST_FILE);
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
    init_arena();
    // phase 1a: parse and local analyse
    for (unsigned i = 0; i < builder.recipe->files.count; i++)
    {
        const char *filename = builder.recipe->files.entries[i];

        if (build_options.verbose)
        {
            LOG(COL_VERBOSE, "parsing (%s) %s", builder.main_component->name, filename);
        }
        init_lexer(read_file(filename));
        bool ok = parse();
        errors += !ok;
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
        //bool ok = builder_check_imports(helper);
        if (build_options.print_timing)
        {
            LOG(COL_TIME, "parsing libraries took %ld ms", (clock() - parse_lib_time) * 1000 / CLOCKS_PER_SEC);
        }
        //if (!ok) goto out;
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
/*            analyser_component_analysis(component, builder->modules /* Diags, context, options.verbose );
            errors += analyser.analyse(options.printAST1, options.printAST2, options.printAST3, options.printASTLib);*/
        }

    }


    out:
    return 0;
}

#if 0
int C2Builder::build() {
    // TODO refactor this function to 'Work'-framework




    ParseHelper helper(Diags, HSOpts, SM, FileMgr, PCMCache, PredefineBuffer, targetInfo);
    if (client->getNumErrors()) goto out;

    uint64_t t1_analyse, t2_analyse;

    t2_analyse = Utils::getCurrentTime();
    if (options.printTiming) log(COL_TIME, "analysis took %" PRIu64" usec", t2_analyse - t1_analyse);

    if (options.printModules) printComponents();
    if (options.printSymbols) printSymbols(options.printLibSymbols);

    if (client->getNumErrors()) goto out;

    if (!checkMainFunction(Diags)) goto out;

    if (!checkExportedPackages()) goto out;

    generateOptionalDeps();

    generateOptionalTags(SM);

    generateInterface();

    generateOptionalC();

    generateOptionalIR();

    if (options.verbose) log(COL_VERBOSE, "done");
out:
    //SM.PrintStats();
    uint64_t t2_build = Utils::getCurrentTime();
    if (options.printTiming) log(COL_TIME, "total build took %" PRIu64" usec", t2_build - t1_build);
    raw_ostream &OS = llvm::errs();
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
}

bool C2Builder::checkImports(ParseHelper& helper) {
    ImportsQueue queue;

    bool ok = true;
    const ModuleList& mainModules = mainComponent->getModules();
    for (unsigned i=0; i<mainModules.size(); i++) {
        ok &= checkModuleImports(helper, mainComponent, mainModules[i], queue);
    }
    if (!ok) return false;

    while (!queue.empty()) {
        std::string currentModuleName = queue.front();
        queue.pop_front();
        const LibInfo* lib = libLoader.findModuleLib(currentModuleName);
        assert(lib);

        ok &= checkModuleImports(helper, lib->component, lib->module, queue, lib);
    }
    return ok;
}

bool C2Builder::checkModuleImports(ParseHelper& helper, Component* component, Module* module, ImportsQueue& queue, const LibInfo* lib) {
    if (!module->isLoaded()) {
        assert(lib);
        if (options.verbose) log(COL_VERBOSE, "parsing (%s) %s", component->getName().c_str(), lib->c2file.c_str());
        if (!helper.parse(*component, module, lib->c2file, (options.printAST0 && options.printASTLib))) {
            return false;
        }
    }
    if (options.verbose) log(COL_VERBOSE, "checking imports for module (%s) %s", component->getName().c_str(), module->getName().c_str());
    bool ok = true;
    const AstList& files = module->getFiles();
    for (unsigned a=0; a<files.size(); a++) {
        AST* ast = files[a];
        for (unsigned u=1; u<ast->numImports(); u++) {  // NOTE: first import is module decl
            ImportDecl* D = ast->getImport(u);
            const std::string& targetModuleName = D->getModuleName();
            // handle c2 pseudo-module
            if (targetModuleName == "c2") {
                createC2Module();
                D->setModule(c2Mod);
                continue;
            }
            const LibInfo* target = libLoader.findModuleLib(targetModuleName);
            if (!target) {
                helper.Diags.Report(D->getLocation(), c2lang::diag::err_unknown_module) << targetModuleName;
                ok = false;
                continue;
            }
            D->setModule(target->module);
            if (target->component != component) {
                // check that imports are in directly dependent component (no indirect component)
                if (!component->hasDep(target->component)) {
                    helper.Diags.Report(D->getLocation(), c2lang::diag::err_indirect_component)
                            << component->getName() << target->component->getName() << targetModuleName;
                    ok = false;
                    continue;
                }
            }

            if (target->module->isLoaded()) continue;
            queue.push_back(targetModuleName);
        }
    }
    return ok;
}

void C2Builder::createC2Module() {
    if (!c2Mod) {
        if (options.verbose) log(COL_VERBOSE, "generating module c2");
        c2Mod = new Module("c2", true, false);
        modules["c2"] = c2Mod;
        C2ModuleLoader::load(c2Mod);
    }
}

C2::Module* C2Builder::findModule(const std::string& name) const {
    ModulesConstIter iter = modules.find(name);
    if (iter == modules.end()) return 0;
    else return iter->second;
}

void C2Builder::printSymbols(bool printLibs) const {
    assert(mainComponent);
    StringBuilder output;
    output.enableColor(true);
    if (printLibs) {
        if (c2Mod) {
            output << "Component <internal>\n";
            c2Mod->printSymbols(output);
        }
        for (unsigned i=0; i<components.size(); i++) {
            components[i]->printSymbols(output);
        }
    } else {
        mainComponent->printSymbols(output);
    }
    printf("%s\n", (const char*)output);
}

void C2Builder::printComponents() const {
    StringBuilder output;
    output.enableColor(true);
    if (c2Mod) {
        output << "Component <internal>\n";
        c2Mod->printFiles(output);
    }
    for (unsigned i=0; i<components.size(); i++) {
        components[i]->print(output);
    }
    printf("%s\n", (const char*)output);
}

void C2Builder::log(const char* color, const char* format, ...) const {
    char buffer[256];
    va_list(Args);
    va_start(Args, format);
    vsprintf(buffer, format, Args);
    va_end(Args);

    if (useColors) printf("%s%s" ANSI_NORMAL "\n", color, buffer);
    else printf("%s\n", buffer);
}

bool C2Builder::checkMainFunction(DiagnosticsEngine& Diags) {
    assert(mainComponent);

    Decl* mainDecl = 0;
    const ModuleList& mods = mainComponent->getModules();
    for (unsigned m=0; m<mods.size(); m++) {
        const Module* M = mods[m];
        Decl* decl = M->findSymbol("main");
        if (decl) {
            if (mainDecl) {
                // TODO multiple main functions
            } else {
                mainDecl = decl;
            }
        }
    }

    if (recipe.type == Component::EXECUTABLE) {
        // bin: must have main
        if (options.testMode) return true;
        if (!mainDecl) {
            Diags.Report(diag::err_main_missing);
            return false;
        }
    } else {
        // lib: cannot have main
        if (mainDecl) {
            Diags.Report(mainDecl->getLocation(), diag::err_lib_has_main);
            return false;
        }
    }
    return true;
}

bool C2Builder::checkExportedPackages() const {
    for (unsigned i=0; i<recipe.exported.size(); i++) {
        const std::string& modName = recipe.exported[i];
        const Module* M = findModule(modName);
        if (!M) {
            fprintf(stderr, "cannot export '%s', no such module\n", modName.c_str());
            return false;
        }
        if (M->isExternal()) {
            fprintf(stderr, "cannot export external module '%s'\n", modName.c_str());
            return false;
        }
    }
    return true;
}


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
