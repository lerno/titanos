cmake_minimum_required(VERSION 3.13)
project(titanos)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_EXTENSIONS true)

include_directories(titanos/src)
include_directories(titanos/src/building)
include_directories(titanos/src/codegen)
include_directories(titanos/src/readers)
include_directories(titanos/src/targets)
include_directories(titanos/src/values)

set(LLVM_LINK_COMPONENTS
        Analysis
        BitReader
        Core
        ExecutionEngine
        InstCombine
        Interpreter
        MC
        MCDisassembler
        MCJIT
        Object
        OrcJIT
        RuntimeDyld
        ScalarOpts
        Support
        Target
        TransformUtils
        native
        nativecodegen
        )

llvm_map_components_to_libnames(llvm_libs support core irreader ${LLVM_LINK_COMPONENTS})

add_executable(titanos
        titanos/src/building/builder.c
        titanos/src/building/builder.h
        titanos/src/building/dependecy.h
        titanos/src/codegen/codegen.c
        titanos/src/codegen/codegen.h
        titanos/src/readers/manifest.c
        titanos/src/readers/manifest.h
        titanos/src/readers/toml.c
        titanos/src/readers/toml.h
        titanos/src/targets/target_info.c
        titanos/src/targets/target_info.h
        titanos/src/ansi_color.h
        titanos/src/arena_allocator.c
        titanos/src/arena_allocator.h
        titanos/src/array.c
        titanos/src/array.h
        titanos/src/ast_types.c
        titanos/src/ast_types.h
        titanos/src/ast_utils.c
        titanos/src/ast_utils.h
        titanos/src/attributes.c
        titanos/src/attributes.h
        titanos/src/common.h
        titanos/src/component.c
        titanos/src/component.h
        titanos/src/constant_folding.c
        titanos/src/constant_folding.h
        titanos/src/debug.c
        titanos/src/debug.h
        titanos/src/diagnostics.c
        titanos/src/diagnostics.h
        titanos/src/error.c
        titanos/src/error.h
        titanos/src/file.c
        titanos/src/file.h
        titanos/src/fnv.c
        titanos/src/fnv.h
        titanos/src/lexer.c
        titanos/src/lexer.h
        titanos/src/module.c
        titanos/src/module.h
        titanos/src/parser.c
        titanos/src/parser.h
        titanos/src/parsing.c
        titanos/src/parsing.h
        titanos/src/printer.h
        titanos/src/scope.c
        titanos/src/scope.h
        titanos/src/scratch_buffer.c
        titanos/src/scratch_buffer.h
        titanos/src/semantic_analysis.c
        titanos/src/semantic_analysis.h
        titanos/src/str_utils.h
        titanos/src/string_utils.h
        titanos/src/symbol.c
        titanos/src/table.c
        titanos/src/table.h
        titanos/src/tests.c
        titanos/src/tests.h
        titanos/src/vector.c
        titanos/src/vector.h
        titanos/src/values/value.c
        titanos/src/values/value.h
        titanos/main.c titanos/src/values/bigint.c titanos/src/values/bigint.h titanos/src/analyser.c titanos/src/analyser.h titanos/src/statement_analysis.c titanos/src/statement_analysis.h titanos/src/types/type.c titanos/src/types/type.h titanos/src/decl.c titanos/src/decl.h titanos/src/type_analysis.c titanos/src/type_analysis.h titanos/src/expr.c titanos/src/expr.h titanos/src/building/builtins.c titanos/src/building/builtins.h titanos/src/expression_analysis.c titanos/src/expression_analysis.h)


target_link_libraries(titanos ${llvm_libs})

