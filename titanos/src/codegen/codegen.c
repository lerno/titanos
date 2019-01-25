#include "codegen.h"
#include "vector.h"
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "module.h"
#include "parser.h"
#include "ast_utils.h"
#include "error.h"

static LLVMTypeRef codegen_convert_type_expr(Ast *type);

static LLVMTypeRef codegen_convert_type(Ast *type)
{
    assert(type->type == AST_TYPE_DEFINITION);
    switch (type->definition.definition_type)
    {
        case STRUCT_TYPE:
            FATAL_ERROR("TODO");
        case FUNC_TYPE:
            FATAL_ERROR("TODO");
        case ENUM_TYPE:
            return codegen_convert_type_expr(type->definition.def_enum.type);
        case ALIAS_TYPE:
            return codegen_convert_type(type->definition.def_alias.type_definition);
        case BUILTIN_TYPE:
            break;
    }
    unsigned bits = type->definition.def_builtin.bits;
    switch (type->definition.def_builtin.type)
    {
        case BUILTIN_UINT:
        case BUILTIN_INT:
            return LLVMIntType(bits);
        case BUILTIN_FLOAT:
            switch (bits)
            {
                case 16:
                    return LLVMHalfType();
                case 32:
                    return LLVMFloatType();
                case 64:
                    return LLVMDoubleType();
                case 128:
                    return LLVMFP128Type();
                default:
                    assert("Illegal bit size");
            }
        case BUILTIN_BOOL:
            return LLVMInt1Type();
    }
    print_ast(type, 0);
    FATAL_ERROR("Illegal type found");
}


static LLVMTypeRef codegen_convert_type_expr(Ast *type)
{
    assert(type->type == AST_TYPE_EXPR && "Expected a type");
    switch (type->type_expr.type)
    {
        case TYPE_EXPR_VOID:
            return LLVMVoidType();
        case TYPE_EXPR_POINTER:
            return LLVMPointerType(codegen_convert_type_expr(type->type_expr.pointer_type_expr.type), 0);
        case TYPE_EXPR_ARRAY:
            if (type->type_expr.array_type_expr.size == NULL)
            {
                return LLVMPointerType(codegen_convert_type_expr(type->type_expr.array_type_expr.type), 0);
            }
            assert(type->type_expr.array_type_expr.size->type == AST_UINT_EXPR
                           || type->type_expr.array_type_expr.size->type == AST_INT_EXPR);
            return LLVMArrayType(codegen_convert_type_expr(type->type_expr.array_type_expr.type),
                                 type->type_expr.array_type_expr.size->type == AST_UINT_EXPR
                                 ? (unsigned)type->type_expr.array_type_expr.size->uint_expr.u
                                 : (unsigned)type->type_expr.array_type_expr.size->int_expr.i);
        case TYPE_EXPR_IDENTIFIER:
            assert(type->type_expr.identifier_type_expr.resolved_type);
            return codegen_convert_type(type->type_expr.identifier_type_expr.resolved_type);

    }
    FATAL_ERROR("Unknown type expression");
}

void codegen_function_proto(LLVMModuleRef llvm_module, Parser *parser, Ast *function)
{
    LLVMTypeRef return_type = codegen_convert_type_expr(function->func_definition.func_decl->func_decl.r_type);


/*
    LLVMFun
    // function part
    // arguments + return type
    llvm::FunctionType *funcType;
    QualType rt = FuncDecl->getReturnType();
    llvm::Type* RT = CGM.ConvertType(rt);
    if (FuncDecl->numArgs() == 0) {
        funcType = llvm::FunctionType::get(RT, FuncDecl->isVariadic());
    } else {
        std::vector<llvm::Type*> Args;
        for (unsigned i=0; i<FuncDecl->numArgs(); i++) {
            VarDecl* arg = FuncDecl->getArg(i);
            QualType qt = arg->getType();
            Args.push_back(CGM.ConvertType(qt));
        }
        llvm::ArrayRef<llvm::Type*> argsRef(Args);
        funcType = llvm::FunctionType::get(RT, argsRef, FuncDecl->isVariadic());
    }
    StringBuilder buffer;
    GenUtils::addName(modName, FuncDecl->getName(), buffer);

    llvm::GlobalValue::LinkageTypes ltype = CGM.getLinkage(FuncDecl->isPublic());
    // override for main
    if (strcmp(FuncDecl->getName(), "main") == 0) ltype = llvm::GlobalValue::ExternalLinkage;

    llvm::Function *func =
        llvm::Function::Create(funcType, ltype, (const char*)buffer, module);

    return func;
*/
}

void codegen_function(LLVMModuleRef llvm_module, Parser *parser, Ast *function)
{

}

void codegen_global_var(LLVMModuleRef llvm_module, Parser *parser, Ast *variable)
{

}

void codegen_file(LLVMModuleRef llvm_module, Parser *parser)
{
}

void codegen(const char *filename, bool single_module, Vector *modules)
{

    LLVMContextRef context = LLVMGetGlobalContext();
    LLVMModuleRef llvm_module = LLVMModuleCreateWithName(filename);
    LLVMSetSourceFileName(llvm_module, filename, strlen(filename));

    for (unsigned m = 0; m < modules->size; m++)
    {
        Module *module = modules->entries[m];
        for (unsigned a = 0; a < module->files->size; a++)
        {
            Parser *parser = module->files->entries[a];
            for (unsigned i = 0; i < parser->functions->size; i++)
            {
                Ast *function = parser->functions->entries[i];
                codegen_function_proto(llvm_module, parser, function);
            }
        }
    }

    for (unsigned m = 0; m < modules->size; m++)
    {
        Module *module = modules->entries[m];
        for (unsigned a = 0; a < module->files->size; a++)
        {
            Parser *parser = module->files->entries[a];
            for (unsigned i = 0; i < parser->functions->size; i++)
            {
                Ast *global = parser->variables->entries[i];
                codegen_global_var(llvm_module, parser, global);
            }
        }
    }

    for (unsigned m = 0; m < modules->size; m++)
    {
        Module *module = modules->entries[m];
        for (unsigned a = 0; a < module->files->size; a++)
        {
            Parser *parser = module->files->entries[a];
            for (unsigned i = 0; i < parser->functions->size; i++)
            {
                Ast *function = parser->functions->entries[i];
                codegen_function(llvm_module, parser, function);
            }
        }
    }

    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMDumpModule(llvm_module);
    /*
     *
    LLVMModuleRef mod = LLVMModuleCreateWithName("my_module");

    LLVMTypeRef param_types[] = { LLVMInt32Type(), LLVMInt32Type() };
    LLVMTypeRef ret_type = LLVMFunctionType(LLVMInt32Type(), param_types, 2, 0);
    LLVMValueRef sum = LLVMAddFunction(mod, "sum", ret_type);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(sum, "entry");

    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMValueRef tmp = LLVMBuildAdd(builder, LLVMGetParam(sum, 0), LLVMGetParam(sum, 1), "tmp");
    LLVMBuildRet(builder, tmp);

    char *error = NULL;
    LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);

    LLVMExecutionEngineRef engine;
    error = NULL;
    LLVMLinkInMCJIT();
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    if (LLVMCreateExecutionEngineForModule(&engine, mod, &error) != 0)
    {
        fprintf(stderr, "failed to create execution engine\n");
        abort();
    }
    if (error)
    {
        fprintf(stderr, "error: %s\n", error);
        LLVMDisposeMessage(error);
        exit(EXIT_FAILURE);
    }


    long long x = 5;
    long long y = 6;

    int (*sum_func)(int, int) = (int (*)(int, int)) LLVMGetFunctionAddress(engine, "sum");
    printf("%d\n", sum_func(x, y));

    // Write out bitcode to file
    if (LLVMWriteBitcodeToFile(mod, "sum.bc") != 0)
    {
        fprintf(stderr, "error writing bitcode to file, skipping\n");
    }

    LLVMDisposeBuilder(builder);
    LLVMDisposeExecutionEngine(engine);*/
}

