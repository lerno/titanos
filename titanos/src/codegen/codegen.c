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
#include <arena_allocator.h>
#include <building/builder.h>
#include "module.h"
#include "parser.h"
#include "error.h"
#include "ast_types.h"
#include "types/type.h"
#include "expr.h"

static LLVMTypeRef llvm_type(Type *type);
static LLVMTypeRef codegen_convert_type(Type *type);
static LLVMTypeRef codegen_convert_decl(Decl *decl);

__thread static Decl *active_func = NULL;
__thread static LLVMContextRef active_context = NULL;
__thread static LLVMBuilderRef active_builder = NULL;

static LLVMTypeRef codegen_convert_func_decl(Decl *decl)
{
    FuncDecl *func_decl = &decl->func_decl;
    LLVMTypeRef return_type = llvm_type(func_decl->rtype);
    LLVMTypeRef* params = (LLVMTypeRef *)malloc_arena(sizeof(LLVMTypeRef) * func_decl->args->size);
    for (unsigned i = 0; i < func_decl->args->size; i++)
    {
        Decl *param_decl = func_decl->args->entries[i];
        params[i] = codegen_convert_decl(param_decl);
    }
    return decl->type.llvm_type = LLVMFunctionType(return_type, params, func_decl->args->size, func_decl->variadic);
}

static LLVMTypeRef codegen_convert_struct_decl(Decl *decl)
{
    StructDecl *struct_decl = &decl->struct_decl;
    LLVMTypeRef* members = (LLVMTypeRef *)malloc_arena(sizeof(LLVMTypeRef) * struct_decl->members->size);
    for (unsigned i = 0; i < struct_decl->members->size; i++)
    {
        Decl *param_decl = struct_decl->members->entries[i];
        members[i] = codegen_convert_decl(param_decl);
    }
    // TODO handle align
    return decl->type.llvm_type = LLVMStructType(members, struct_decl->members->size, false);
}

static LLVMTypeRef codegen_convert_decl(Decl *decl)
{
    assert(!decl->type.llvm_type);
    switch (decl->type_id)
    {
        case DECL_BUILTIN:
        case DECL_ENUM_CONSTANT:
            FATAL_ERROR("LLVM type should already have been generated");
        case DECL_FUNC:
            return codegen_convert_func_decl(decl);
        case DECL_VAR:
            return codegen_convert_type(decl->var.type);
        case DECL_ALIAS_TYPE:
            return codegen_convert_type(decl->alias_decl.type);
        case DECL_STRUCT_TYPE:
            return codegen_convert_struct_decl(decl);
        case DECL_ENUM_TYPE:
            return codegen_convert_type(decl->enum_decl.type);
        case DECL_FUNC_TYPE:
            return codegen_convert_decl(decl->func_type.func_decl);
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
            FATAL_ERROR("Invalid use of decl %d", decl->type_id);
    }
    UNREACHABLE;
}

static LLVMTypeRef codegen_convert_type(Type *type)
{
    assert(!type->llvm_type && "Called even though the type is already set.");

    switch (type->type_id)
    {
        case TYPE_VOID:
            return type->llvm_type = LLVMVoidType();
        case TYPE_POINTER:
            return type->llvm_type = LLVMPointerType(llvm_type(type->pointer.base), 0);
        case TYPE_DECLARED:
            return type->llvm_type = codegen_convert_decl(type->decl);
        case TYPE_ARRAY:
            return type->llvm_type = LLVMArrayType(llvm_type(type->array.base), type->array.len);
        case TYPE_NIL:
            return type->llvm_type = LLVMPointerType(LLVMVoidType(), 0);
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

void codegen_function_proto(LLVMModuleRef llvm_module, Parser *parser, Decl *function)
{
    __thread static char buffer[128];
    strncpy(buffer, function->name.start, MIN(function->name.length, 127));
    LLVMValueRef fun = LLVMAddFunction(llvm_module, buffer, llvm_type(&function->type));
    bool single_module = false;
    bool external = (function->is_public && !single_module) || token_compare_str(&function->name, "main");
    LLVMSetLinkage(fun, external ? LLVMExternalLinkage : LLVMLinkerPrivateLinkage);
    function->func_decl.llvm_function_proto = fun;
}

LLVMValueRef codegen_const_expr(Expr *expr)
{
    assert(expr->expr_id == EXPR_CONST);
    switch (expr->type->type_id)
    {
        case TYPE_INVALID:break;
        case TYPE_VOID:break;
        case TYPE_POINTER:break;
        case TYPE_ARRAY:break;
        case TYPE_DECLARED:break;
        case TYPE_CONST_FLOAT:
            break;
        case TYPE_CONST_INT:break;
        case TYPE_NIL:break;
        case TYPE_OPAQUE:break;
        case TYPE_IMPORT:break;
        case TYPE_UNRESOLVED:break;
        case TYPE_TYPEVAL:break;
        case TYPE_STRING:break;
        case TYPE_BUILTIN:
            switch (expr->type->builtin.builtin_kind)
            {
                case BUILTIN_UNSIGNED_INT:
                    return LLVMConstInt(expr->type->llvm_type, bigint_as_unsigned(&expr->const_expr.value.big_int), false);
                case BUILTIN_SIGNED_INT:
                    return LLVMConstInt(expr->type->llvm_type, (uint64_t)bigint_as_signed(&expr->const_expr.value.big_int), true);
                case BUILTIN_FLOAT:
                    // TODO
                    return LLVMConstReal(expr->type->llvm_type, expr->const_expr.value.f);
                case BUILTIN_BOOL:
                    return LLVMConstInt(expr->type->llvm_type, expr->const_expr.value.b ? 1 : 0, false);
            }
            UNREACHABLE;
    }
    return NULL;
}
LLVMValueRef codegen_expr(Expr *expr)
{
    switch (expr->expr_id)
    {
        case EXPR_CONST:
            return codegen_const_expr(expr);
            break;
        case EXPR_TYPE:break;
        case EXPR_BINARY:break;
        case EXPR_TERNARY:break;
        case EXPR_UNARY:break;
        case EXPR_POST:break;
        case EXPR_IDENTIFIER:break;
        case EXPR_CALL:break;
        case EXPR_SIZEOF:break;
        case EXPR_CAST:break;
        case EXPR_SUBSCRIPT:break;
        case EXPR_ACCESS:break;
        case EXPR_STRUCT_INIT_VALUES:break;
        case EXPR_DESIGNATED_INITIALIZER:break;
    }
    printf("Not implemented expr");
    return NULL;
}

LLVMValueRef codegen_declare(Decl *decl)
{
    assert(decl->type_id == DECL_VAR && decl->var.kind == VARDECL_LOCAL);
    LLVMValueRef val = NULL;
    decl->var.llvm_ref = LLVMBuildAlloca(active_builder, llvm_type(decl->var.type), "");
    LLVMSetValueName2(decl->var.llvm_ref, decl->name.start, decl->name.length);
    if (decl->var.init_expr)
    {
        val = codegen_expr(decl->var.init_expr);
        if (val)
        {
            DEBUG_LOG("Got here!");
            LLVMBuildStore(active_builder, val, decl->var.llvm_ref);
        }
    }
    return val;
}
void codegen_block(Ast *block)
{
    LOG_FUNC

    assert(block->ast_id == AST_COMPOUND_STMT);
    LLVMValueRef lastval = NULL; // Should never be used by caller
    for (unsigned i = 0; i < block->compound_stmt.stmts->size; i++)
    {
        Ast *stmt = block->compound_stmt.stmts->entries[i];
        switch (stmt->ast_id)
        {
            case AST_EXPR_STMT:
                codegen_expr(stmt->expr_stmt.expr);
                break;
            case AST_DECLARE_STMT:
                codegen_declare(stmt->declare_stmt.decl);
                break;
            case AST_ATTRIBUTE:
                UNREACHABLE
                break;
            case AST_COMPOUND_STMT:
            case AST_IF_STMT:
            case AST_WHILE_STMT:
            case AST_DO_STMT:
            case AST_DEFER_STMT:
            case AST_SWITCH_STMT:
            case AST_CASE_STMT:
            case AST_DEFAULT_STMT:
            case AST_BREAK_STMT:
            case AST_CONTINUE_STMT:
            case AST_RETURN_STMT:
            case AST_GOTO_STMT:
            case AST_FOR_STMT:
            case AST_LABEL:
            case AST_DEFER_RELASE:
            case AST_ASM_STMT:
                printf("Not implemented\n");
                continue;
        }
    }
}
void codegen_parameter_var(Decl *param)
{
    TODO
}

void codegen_function(LLVMModuleRef llvm_module, Parser *parser, Decl *function)
{
    LOG_FUNC

    assert(!active_func);
    LLVMBuilderRef previous_builder = active_builder;

    active_func = function;

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(active_context, active_func->func_decl.llvm_function_proto, "entry");

    active_builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(active_builder, entry);


    // Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
    uint32_t cnt;

    for (unsigned i = 0; i < function->func_decl.args->size; i++)
    {
        codegen_parameter_var(function->func_decl.args->entries[i]);
    }

    codegen_block(function->func_decl.body);

    LLVMDisposeBuilder(active_builder);

    active_builder = previous_builder;

    active_func = NULL;


}

void codegen_global_var(LLVMModuleRef llvm_module, Parser *parser, Ast *variable)
{

}

void codegen_file(LLVMModuleRef llvm_module, Parser *parser)
{
}

void codegen(const char *filename, bool single_module, Vector *modules)
{
    active_context = LLVMGetGlobalContext();
    LLVMModuleRef llvm_module = LLVMModuleCreateWithNameInContext(filename, active_context);
    LLVMSetSourceFileName(llvm_module, filename, strlen(filename));

    for (unsigned m = 0; m < modules->size; m++)
    {
        Module *module = modules->entries[m];
        for (unsigned a = 0; a < module->files->size; a++)
        {
            Parser *parser = module->files->entries[a];
            for (unsigned i = 0; i < parser->functions->size; i++)
            {
                Decl *function = parser->functions->entries[i];
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
                Decl *function = parser->functions->entries[i];
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

