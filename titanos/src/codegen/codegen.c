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
#include "error.h"
#include "ast_types.h"
#include "types/type.h"


static LLVMTypeRef llvm_type(Type *type);

static LLVMTypeRef codegen_convert_type(Type *type)
{
    assert(!type->llvm_type && "Called even though the type is already set.");

    switch (type->type_id)
    {
        case TYPE_VOID:
            return LLVMVoidType();
        case TYPE_POINTER:
            return LLVMPointerType(codegen_convert_type(type->pointer.base), 0);
        case TYPE_BOOL:
            return LLVMInt1Type();
        case TYPE_INT:
            return LLVMIntType(type->integer.bits);
        case TYPE_ARRAY:
            return LLVMArrayType(codegen_convert_type(type->array.base), type->array.len);
        case TYPE_FLOAT:
            switch (type->real.bits)
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
                    FATAL_ERROR("Illegal floating point size %d", type->real.bits);
            }
        case TYPE_NIL:
            return LLVMPointerType(LLVMVoidType(), 0);
        case TYPE_FUNC:
            assert(type->func.rtype_resolved);
//            return LLVMFunctionType(llvm_type(type->func.rtype), /* TODO */ NULL, type->func.params->size, type->func.params->param_list.variadic);
        case TYPE_ENUM:
            return llvm_type(type->enumeration.int_type);
        case TYPE_STRUCT:
        case TYPE_UNION:
            FATAL_ERROR("TODO");
//            return llvm_type(...)
        case TYPE_INVALID:
            FATAL_ERROR("Invalid type reached");
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
            FATAL_ERROR("Should never happen");
        default:
            FATAL_ERROR("Unknown type %d", type->type_id);
    }
}

static LLVMTypeRef llvm_type(Type *type)
{
    if (type->llvm_type) return type->llvm_type;
    type->llvm_type = codegen_convert_type(type);
    return type->llvm_type;
}

void codegen_function_proto(LLVMModuleRef llvm_module, Parser *parser, Ast *function)
{
//    LLVMTypeRef return_type = codegen_convert_type_expr(function->func_definition.func_decl->func.rtype);


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

