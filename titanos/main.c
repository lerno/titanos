//
//  main.c
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include "file.h"
#include "lexer.h"
#include "tests.h"
#include "builder.h"
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>

static void usage(const char* name) {
    fprintf(stderr, "Usage: %s <options> <target>\n", name);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "   -a0           - print AST after parsing\n");
    fprintf(stderr, "   -a1           - print AST after analysis 1\n");
    fprintf(stderr, "   -a2           - print AST after analysis 2\n");
    fprintf(stderr, "   -a3           - print AST after analysis 3 (final)\n");
    fprintf(stderr, "   -aL           - also print library AST\n");
    fprintf(stderr, "   -b <file>     - use specified build file\n");
    fprintf(stderr, "   -d <dir>      - change directory first\n");
    fprintf(stderr, "   -f <file>     - compile single file without recipe\n");
    fprintf(stderr, "   -h            - show this help\n");
    fprintf(stderr, "   -i            - generate LLVM IR code\n");
    fprintf(stderr, "   -I            - generate + print LLVM IR code\n");
    fprintf(stderr, "   -l            - list targets\n");
    fprintf(stderr, "   -p            - print all modules\n");
    fprintf(stderr, "   -s            - print symbols (excluding library symbols)\n");
    fprintf(stderr, "   -S            - print symbols (including library symbols)\n");
    fprintf(stderr, "   -t            - print timing\n");
    fprintf(stderr, "   -v            - verbose logging\n");
    fprintf(stderr, "   --about       - print information about C2 and c2c\n");
    fprintf(stderr, "   --test        - test mode (don't check for main())\n");
    fprintf(stderr, "   --deps        - print module dependencies\n");
    fprintf(stderr, "   --refs        - generate c2tags file\n");
    fprintf(stderr, "   --check       - only parse + analyse\n");
    fprintf(stderr, "   --showlibs    - print available libraries\n");
    fprintf(stderr, "   --testsuite   - do some testing\n");
    fprintf(stderr, "   --lib <dir>   - use this directory as the c@2 library path\n");
    exit(EXIT_SUCCESS);
}

static void run_tests()
{
    // insert code here...
    test_all();
}

static void parse_arguments(int argc, const char *argv[])
{
    for (int i = 1; i < argc; i++)
    {
        const char *arg = argv[i];
        if (arg[0] == '-')
        {
            switch (arg[1])
            {
                case 'a':
                    switch (arg[2])
                    {
                        case '0':
                            build_options.print_ast_0 = true;
                            break;
                        case '1':
                            build_options.print_ast_1 = true;
                            break;
                        case '2':
                            build_options.print_ast_2 = true;
                            break;
                        case '3':
                            build_options.print_ast_3 = true;
                            break;
                        case 'L':
                            build_options.print_ast_lib = true;
                            break;
                        default:
                            usage(argv[0]);
                            break;
                    }
                    break;
                case 'b':
                    if (i == argc - 1)
                    {
                        fprintf(stderr, "error: -b needs an argument\n");
                        exit(EXIT_FAILURE);
                    }
                    i++;
                    build_options.build_file = argv[i];
                    break;
                case 'd':
                    if (i == argc - 1)
                    {
                        fprintf(stderr, "error: -d needs an argument\n");
                        exit(EXIT_FAILURE);
                    }
                    i++;
                    build_options.other_dir = argv[i];
                    break;
                case 'f':
                    build_options.skip_recipe = true;
                    break;
                case 'h':
                    usage(argv[0]);
                    break;
                case 'i':
                    build_options.generate_ir = true;
                    break;
                case 'I':
                    build_options.generate_ir = true;
                    build_options.print_ir = true;
                    break;
                case 'l':
                    build_options.print_targets = true;
                    break;
                case 'p':
                    build_options.print_modules = true;
                    break;
                case 's':
                    build_options.print_symbols = true;
                    break;
                case 'S':
                    build_options.print_symbols = true;
                    build_options.print_lib_symbols = true;
                    break;
                case 't':
                    build_options.print_timing = true;
                    break;
                case 'v':
                    build_options.verbose = true;
                    break;
                case '-':
                    if (strcmp(&arg[2], "about") == 0)
                    {
                        fprintf(stderr, "The C@2 Compiler\n");
                        fprintf(stderr, "\nC@2 is a programming language aiming to keep the good of C and remove/improve its\n");
                        fprintf(stderr, "bad parts. It provides stricter syntax, great tooling, better compilation times\n");
                        fprintf(stderr, "than C, easy debugging, smart integrated build system, friendly and readable\n");
                        fprintf(stderr, "syntax, requires less typing than C and allows higher development speed.\n");
                        fprintf(stderr, " Its aim is to be used for problems where currently C would be used. So low-\n");
                        fprintf(stderr, "level programs, like bootloaders, kernels, drivers and system-level tooling.\n");
                        fprintf(stderr, "\nC@2 is based on C2.\nSee c2lang.org for more information\n");
                        exit(0);
                    }
                    if (strcmp(&arg[2], "test") == 0)
                    {
                        build_options.test_mode = true;
                        continue;
                    }
                    if (strcmp(&arg[2], "deps") == 0)
                    {
                        build_options.print_dependencies = true;
                        continue;
                    }
                    if (strcmp(&arg[2], "refs") == 0)
                    {
                        build_options.generate_refs = true;
                        continue;
                    }
                    if (strcmp(&arg[2], "lib") == 0)
                    {
                        if (i == argc - 1)
                        {
                            fprintf(stderr, "error: --lib needs a directory\n");
                            exit(EXIT_FAILURE);
                        }
                        i++;
                        build_options.lib_dir = argv[i];
                        continue;
                    }
                    if (strcmp(&arg[2], "check") == 0)
                    {
                        build_options.check_only = true;
                        continue;
                    }
                    if (strcmp(&arg[2], "showlibs") == 0)
                    {
                        build_options.show_libs = true;
                        continue;
                    }
                    if (strcmp(&arg[2], "testsuite") == 0)
                    {
                        run_tests();
                        exit(EXIT_SUCCESS);
                    }
                    usage(argv[0]);
                    break;
                default:
                    usage(argv[0]);
                    break;
            }
        }
        else
        {
            if (build_options.target_filter)
            {
                usage(argv[0]);
            }
            build_options.target_filter = arg;
        }
    }
    if (build_options.skip_recipe)
    {
        if (!build_options.target_filter)
        {
            fprintf(stderr, "error: argument -f needs a filename\n");
            exit(EXIT_FAILURE);
        }
        if (build_options.print_targets)
        {
            fprintf(stderr, "error: -f cannot be used together with -l\n");
            exit(EXIT_FAILURE);
        }
        if (build_options.build_file)
        {
            fprintf(stderr, "error: -f cannot be used together with -b\n");
            exit(EXIT_FAILURE);
        }
    }
}

char base_path[PATH_MAX];
char rel_path[PATH_MAX];
char *path_prefix;

void find_top_dir()
{
    char *path;
    char buffer[PATH_MAX];
    while (1)
    {
        path = getcwd(buffer, PATH_MAX);
        if (path == 0)
        {
            perror("getcwd");
            exit(EXIT_FAILURE);
        }
        struct stat buf;
        int error = stat(RECIPE_FILE, &buf);
        if (error)
        {
            if (buffer[0] == '/' && buffer[1] == 0)
            {
                fprintf(stderr, "c@2c: error: cannot find C@2 root dir\n");
                fprintf(stderr, "c@2c requires a %s file for compilation of targets\n", RECIPE_FILE);
                fprintf(stderr, "Use argument -h for a list of available options and usage of c@2c\n");
                exit(EXIT_FAILURE);
            }
            if (errno != ENOENT)
            {
                perror("stat");
                exit(EXIT_FAILURE);
            }
        }
        else
        {
            // must be file, not dir
            if (S_ISREG(buf.st_mode))
            {
                path_prefix = base_path + strlen(buffer);
                if (*path_prefix == '/') path_prefix++;
                return;
            }
        }
        error = chdir("..");
        if (error)
        {
            perror("chdir");
            exit(EXIT_FAILURE);
        }
        strcat(rel_path, "../");
    }
}

int main(int argc, const char *argv[])
{
    // uint64_t t1 = Utils::getCurrentTime();
    parse_arguments(argc, argv);

    build_options.lib_dir = build_options.lib_dir ? build_options.lib_dir : getenv("C2_LIBDIR");
    if (!build_options.lib_dir) printf("Warning: environment variable C2_LIBDIR not set!\n");
    if (build_options.other_dir)
    {
        if (chdir(build_options.other_dir))
        {
            fprintf(stderr, "cannot chdir to %s: %s\n", build_options.other_dir, strerror(errno));
            return EXIT_FAILURE;
        }
    }
    if (build_options.skip_recipe)
    {
        Recipe *dummy = recipe_create();
        dummy->name = "dummy";
        dummy->type = COMPONENT_TYPE_EXECUTABLE;
        recipe_add_file(dummy, build_options.target_filter);

        int errors = build_recipe(dummy, NULL);
        return errors ? EXIT_FAILURE : EXIT_SUCCESS;
    }

    find_top_dir();

    /*
    RecipeReader reader;
    if (print_targets) {
        reader.print();
        return EXIT_SUCCESS;
    }

    BuildFile buildFile;
    BuildFile* buildFilePtr = NULL;
    BuildFileReader buildReader(buildFile);
    if (!build_file) build_file = finder.getBuildFile();
    if (build_file) {
        if (!buildReader.parse(build_file)) {
            fprintf(stderr, "Error reading %s: %s\n", build_file, buildReader.getErrorMsg());
            return EXIT_FAILURE;
        }
        buildFilePtr = &buildFile;
    }

    int count = 0;
    bool hasErrors = false;
    for (int i=0; i<reader.count(); i++) {
        const Recipe& recipe = reader.get(i);
        if (targetFilter && recipe.name != targetFilter) continue;
        C2Builder builder(recipe, buildFilePtr, opts);
        int errors = builder.checkFiles();
        if (!errors) errors = builder.build();
        if (errors) hasErrors = true;
        count++;
    }
    if (targetFilter && count == 0) {
        fprintf(stderr, "error: unknown target '%s'\n", targetFilter);
        return EXIT_FAILURE;
    }
    if (opts.printTiming) {
        uint64_t t2 = Utils::getCurrentTime();
        printf(COL_TIME"total building time: %" PRIu64" usec" ANSI_NORMAL"\n", t2 - t1);
    }
*/
    bool hasErrors = false;
    return hasErrors ? EXIT_FAILURE : EXIT_SUCCESS;

}
