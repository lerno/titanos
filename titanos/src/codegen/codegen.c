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
#include <building/builtins.h>
#include "module.h"
#include "parser.h"
#include "error.h"
#include "ast_types.h"
#include "types/type.h"
#include "expr.h"

static LLVMTypeRef llvm_type(Type *type);
static LLVMTypeRef codegen_convert_type(Type *type);
static LLVMTypeRef codegen_convert_decl(Decl *decl);
static LLVMValueRef codegen_expr(Expr *expr);
static void codegen_block(Ast *block);

__thread static Decl *active_func = NULL;
__thread static LLVMContextRef active_context = NULL;
__thread static LLVMBuilderRef active_builder = NULL;
__thread static LLVMBasicBlockRef active_break = NULL;
__thread static LLVMBasicBlockRef active_continue = NULL;

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

LLVMBasicBlockRef codegen_insert_block(char *name)
{
    LLVMBasicBlockRef nextblock = LLVMGetNextBasicBlock(LLVMGetInsertBlock(active_builder));
    if (nextblock)
    {
        return LLVMInsertBasicBlockInContext(active_context, nextblock, name);
    }
    else
    {
        return LLVMAppendBasicBlockInContext(active_context, active_func->func_decl.llvm_function_proto, name);
    }

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
        case TYPE_CONST_INT:
            // TODO this is just for test.
            return LLVMConstInt(type_builtin_i32()->llvm_type, bigint_as_unsigned(&expr->const_expr.value.big_int), false);
        case TYPE_OPAQUE:break;
        case TYPE_IMPORT:
            break;
        case TYPE_UNRESOLVED:break;
        case TYPE_TYPEVAL:break;
        case TYPE_STRING:break;
        case TYPE_BUILTIN:
            switch (expr->type->builtin.builtin_id)
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
        case TYPE_NIL:break;
    }
    return NULL;
}
LLVMValueRef perform_expr(LLVMValueRef left, LLVMValueRef right, token_type token, bool use_float, bool use_signed)
{
    switch (token)
    {
        case TOKEN_EQ:
            return LLVMBuildStore(active_builder, right, left);
        case TOKEN_PLUS:
            return use_float
                   ? LLVMBuildFAdd(active_builder, left, right, "addf")
                   : LLVMBuildAdd(active_builder, left, right, "add");
        case TOKEN_MINUS:
            return use_float
                   ? LLVMBuildFSub(active_builder, left, right, "subf")
                   : LLVMBuildSub(active_builder, left, right, "sub");
        case TOKEN_PLUS_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_PLUS, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_BIT_NOT:
            assert(!use_float);
            assert(!right && "Cannot have right hand side");
            return LLVMBuildNot(active_builder, left, "bit-not");
        case TOKEN_NOT:
            return LLVMBuildXor(active_builder, left, LLVMConstInt(LLVMInt1TypeInContext(active_context), 1, 0), "not");
        case TOKEN_PLUSPLUS:
        case TOKEN_MINUSMINUS:
            FATAL_ERROR("Should be handled separately");
        case TOKEN_MINUS_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_MINUS, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_STAR:
            if (!right)
            {
                // Deref
                TODO;
            }
            return use_float
                   ? LLVMBuildFMul(active_builder, left, right, "mulf")
                   : LLVMBuildMul(active_builder, left, right, "mul");
        case TOKEN_MULT_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_STAR, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_POW:break;
        case TOKEN_DIV:
            return use_float
                   ? LLVMBuildFDiv(active_builder, left, right, "divf")
                   : (use_signed ? LLVMBuildSDiv(active_builder, left, right, "sdiv") : LLVMBuildUDiv(active_builder, left, right, "udiv"));
        case TOKEN_DIV_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_DIV, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_MOD:
            return use_float
                   ? LLVMBuildFRem(active_builder, left, right, "remf")
                   : (use_signed ? LLVMBuildSRem(active_builder, left, right, "srem") : LLVMBuildURem(active_builder, left, right, "urem"));
        case TOKEN_MOD_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_MOD, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_NOT_EQUAL:
            return use_float
                   ? LLVMBuildFCmp(active_builder, LLVMRealONE, left, right, "!=")
                   : LLVMBuildICmp(active_builder, LLVMIntNE, left, right, "!=");
        case TOKEN_EQEQ:
            return use_float
                   ? LLVMBuildFCmp(active_builder, LLVMRealOEQ, left, right, "eq")
                   : LLVMBuildICmp(active_builder, LLVMIntEQ, left, right, "eq");
        case TOKEN_GREATER:
            return use_float
                   ? LLVMBuildFCmp(active_builder, LLVMRealOGT, left, right, "gt")
                   : LLVMBuildICmp(active_builder, use_signed ? LLVMIntSGT : LLVMIntUGT, left, right, "gt");
        case TOKEN_GREATER_EQ:
            return use_float
                   ? LLVMBuildFCmp(active_builder, LLVMRealOGE, left, right, "ge")
                   : LLVMBuildICmp(active_builder, use_signed ? LLVMIntSGE : LLVMIntUGE, left, right, "ge");
        case TOKEN_RIGHT_SHIFT:
            assert(!use_float);
            return use_signed ? LLVMBuildAShr(active_builder, left, right, "shr")
                              : LLVMBuildLShr(active_builder, left, right, "shrl");
        case TOKEN_RIGHT_SHIFT_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_RIGHT_SHIFT, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_LESS:
            return use_float
                   ? LLVMBuildFCmp(active_builder, LLVMRealOLT, left, right, "lt")
                   : LLVMBuildICmp(active_builder, use_signed ? LLVMIntSLT : LLVMIntULT, left, right, "lt");
        case TOKEN_LESS_EQ:
            return use_float
                   ? LLVMBuildFCmp(active_builder, LLVMRealOLE, left, right, "le")
                   : LLVMBuildICmp(active_builder, use_signed ? LLVMIntSLE : LLVMIntULE, left, right, "le");
        case TOKEN_LEFT_SHIFT:
            assert(!use_float);
            return LLVMBuildShl(active_builder, left, right, "shl");
        case TOKEN_LEFT_SHIFT_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_LEFT_SHIFT, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_AND:break;
        case TOKEN_AND_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_AND, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_AMP:
            assert(!use_float);
            return LLVMBuildAnd(active_builder, left, right, "bit-and");
        case TOKEN_BIT_AND_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_AMP, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_OR:break;
        case TOKEN_OR_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_OR, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_BIT_OR:
            return LLVMBuildOr(active_builder, left, right, "bit or");
        case TOKEN_BIT_OR_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_BIT_OR, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_BIT_XOR:
            assert(!use_float);
            return LLVMBuildXor(active_builder, left, right, "xor");
        case TOKEN_BIT_XOR_ASSIGN:
            return perform_expr(left, perform_expr(left, right, TOKEN_BIT_XOR, use_float, use_signed), TOKEN_EQ, use_float, use_signed);
        case TOKEN_COLON:
        case TOKEN_COLON_ASSIGN:
        case TOKEN_COLCOLON:
        case TOKEN_DOTDOT:
        case TOKEN_ELIPSIS:
        case TOKEN_NO_INIT:
        case TOKEN_IDENTIFIER:
        case TOKEN_ARROW:
        case TOKEN_STRING:
        case TOKEN_INTEGER:
        case TOKEN_DOT:
        case TOKEN_FLOAT:
        case TOKEN_QUESTION:
        case TOKEN_ELVIS:
        case TOKEN_VOID:
        case TOKEN_ALIAS:
        case TOKEN_CONST:
        case TOKEN_VOLATILE:
        case TOKEN_ELSE:
        case TOKEN_FALSE:
        case TOKEN_CONTINUE:
        case TOKEN_FUNC:
        case TOKEN_FOR:
        case TOKEN_IMPORT:
        case TOKEN_MODULE:
        case TOKEN_IF:
        case TOKEN_NIL:
        case TOKEN_RETURN:
        case TOKEN_GOTO:
        case TOKEN_DEFER:
        case TOKEN_TRUE:
        case TOKEN_WHILE:
        case TOKEN_LPAREN:
        case TOKEN_RPAREN:
        case TOKEN_LBRACE:
        case TOKEN_RBRACE:
        case TOKEN_LBRACKET:
        case TOKEN_RBRACKET:
        case TOKEN_HASH:
        case TOKEN_DOLLAR:
        case TOKEN_COMMA:
        case TOKEN_EOS:
        case TOKEN_CASE:
        case TOKEN_ASM:
        case TOKEN_DEFAULT:
        case TOKEN_CAST:
        case TOKEN_SWITCH:
        case TOKEN_UNTIL:
        case TOKEN_BREAK:
        case TOKEN_TYPE:
        case TOKEN_DO:
        case TOKEN_PUBLIC:
        case TOKEN_LOCAL:
        case TOKEN_STRUCT:
        case TOKEN_UNION:
        case TOKEN_ENUM:
        case TOKEN_SIZEOF:
        case TOKEN_AT:
        case TOKEN_AS:
        case TOKEN_ERROR:
        case TOKEN_EOF:
            UNREACHABLE
    }
    TODO;
}

static inline bool is_float(Expr *expr)
{
    return expr->type->type_id == TYPE_BUILTIN && expr->type->builtin.builtin_id == BUILTIN_FLOAT;
}

static inline bool is_signed(Expr *expr)
{
    return expr->type->type_id == TYPE_BUILTIN && expr->type->builtin.builtin_id == BUILTIN_SIGNED_INT;
}

LLVMValueRef codegen_binary_expr(Expr *expr)
{
    assert(expr->expr_id == EXPR_BINARY);
    LLVMValueRef left = codegen_expr(expr->binary_expr.left);
    LLVMValueRef right = codegen_expr(expr->binary_expr.right);
    bool use_float = is_float(expr->binary_expr.left);
    bool use_signed = use_float || is_signed(expr->binary_expr.left);
    return perform_expr(left, right, expr->binary_expr.operator, use_float, use_signed);
}

LLVMValueRef codegen_unary_expr(Expr *expr)
{
    assert(expr->expr_id == EXPR_UNARY);
    bool use_float = is_float(expr->unary_expr.expr);
    bool use_signed = use_float || is_signed(expr->unary_expr.expr);
    return perform_expr(codegen_expr(expr->unary_expr.expr), NULL, expr->unary_expr.operator, use_float, use_signed);
}

LLVMValueRef codegen_post_expr(Expr *expr)
{
    assert(expr->expr_id == EXPR_UNARY);
    bool use_float = is_float(expr->post_expr.expr);
    bool use_signed = use_float || is_signed(expr->post_expr.expr);
    return perform_expr(NULL, codegen_expr(expr->post_expr.expr), expr->post_expr.operator, use_float, use_signed);
}

LLVMValueRef codegen_cast(Expr *expr)
{
    assert(expr->expr_id == EXPR_CAST);
    LLVMTypeRef type = llvm_type(expr->cast_expr.type);
    LLVMValueRef value = codegen_expr(expr->cast_expr.expr);

    Type *target_type = type_unfold_opaque(expr->cast_expr.type);
    Type *source_type = type_unfold_opaque(expr->cast_expr.expr->type);
    switch (expr->cast_expr.cast_result)
    {
        case CAST_INLINE:
        case CAST_FAILED:
            UNREACHABLE
        case CAST_PTRPTR:
            return LLVMBuildPointerCast(active_builder, value, type, "");
        case CAST_INTPTR:
            return LLVMBuildIntToPtr(active_builder, value, type, "");
        case CAST_PTRINT:
            return LLVMBuildPtrToInt(active_builder, value, type, "");
        case CAST_FPFP:
            if (target_type->builtin.bits > source_type->builtin.bits)
            {
                return LLVMBuildFPExt(active_builder, value, type, "");
            }
            if (target_type->builtin.bits < source_type->builtin.bits)
            {
                return LLVMBuildFPTrunc(active_builder, value, type, "");
            }
            assert(false && "FP cast to same type");
            return value;
        case CAST_FPUI:
            return LLVMBuildFPToUI(active_builder, value, type, "");
        case CAST_FPSI:
            return LLVMBuildFPToUI(active_builder, value, type, "");
        case CAST_UIFP:
            return LLVMBuildUIToFP(active_builder, value, type, "");
        case CAST_UIUI:
            if (target_type->builtin.bits > source_type->builtin.bits)
            {
                return LLVMBuildZExt(active_builder, value, type, "");
            }
            if (target_type->builtin.bits < source_type->builtin.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            assert(false && "UI cast to same type");
            return value;
        case CAST_UISI:
            if (target_type->builtin.bits > source_type->builtin.bits)
            {
                return LLVMBuildZExt(active_builder, value, type, "");
            }
            if (target_type->builtin.bits < source_type->builtin.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            // If same bit size, this is a noop, since LLVM ignores sign.
            return value;
        case CAST_SIFP:
            return LLVMBuildSIToFP(active_builder, value, type, "");
        case CAST_SISI:
            if (target_type->builtin.bits > source_type->builtin.bits)
            {
                return LLVMBuildSExt(active_builder, value, type, "");
            }
            if (target_type->builtin.bits < source_type->builtin.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            assert(false && "SI cast to same type");
            return value;
        case CAST_SIUI:
            if (target_type->builtin.bits > source_type->builtin.bits)
            {
                return LLVMBuildSExt(active_builder, value, type, "");
            }
            if (target_type->builtin.bits > source_type->builtin.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            // If same bit size, this is a noop, since LLVM ignores sign.
            return value;
    }
    UNREACHABLE
}

static inline LLVMValueRef codegen_ternary_expr(Expr *expr)
{
    assert(expr->expr_id == EXPR_TERNARY);
    LLVMValueRef select = codegen_expr(expr->ternary_expr.cond);
    LLVMValueRef true_res = codegen_expr(expr->ternary_expr.then_expr);
    LLVMValueRef false_res = codegen_expr(expr->ternary_expr.else_expr);
    return LLVMBuildSelect(active_builder, select, true_res, false_res, "");
}

static LLVMValueRef codegen_expr(Expr *expr)
{
    switch (expr->expr_id)
    {
        case EXPR_CONST:
            return codegen_const_expr(expr);
        case EXPR_TYPE:
            FATAL_ERROR("Type should never leak to codegen");
        case EXPR_BINARY:
            return codegen_binary_expr(expr);
        case EXPR_TERNARY:
            return codegen_ternary_expr(expr);
        case EXPR_UNARY:
            return codegen_unary_expr(expr);
        case EXPR_POST:
            return codegen_post_expr(expr);
        case EXPR_IDENTIFIER:
            assert(expr->identifier_expr.resolved->var.llvm_ref);
            return expr->identifier_expr.resolved->var.llvm_ref;
        case EXPR_SIZEOF:
            FATAL_ERROR("Sizeof should always be const evaluated");
        case EXPR_CAST:
            return codegen_cast(expr);
        case EXPR_CALL:break;
        case EXPR_SUBSCRIPT:break;
        case EXPR_ACCESS:break;
        case EXPR_STRUCT_INIT_VALUES:break;
        case EXPR_DESIGNATED_INITIALIZER:break;
    }
    printf("Not implemented expr\n");
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

static inline void codegen_while(Ast *stmt)
{
    // Store break and continue statements
    LLVMBasicBlockRef prev_break = active_break;
    LLVMBasicBlockRef prev_continue = active_continue;

    LLVMBasicBlockRef while_end = active_break = codegen_insert_block("whileend");
    LLVMBasicBlockRef while_block = codegen_insert_block("whileblock");
    LLVMBasicBlockRef while_start = active_continue = codegen_insert_block("whilebegin");

    LLVMBuildBr(active_builder, while_start);
    LLVMPositionBuilderAtEnd(active_builder, while_start);
    LLVMBuildCondBr(active_builder, codegen_expr(stmt->while_stmt.expr), while_block, while_end);

    codegen_block(stmt->while_stmt.body);

    LLVMBuildBr(active_builder, while_start);
    LLVMPositionBuilderAtEnd(active_builder, while_end);

    active_break = prev_break;
    active_continue = prev_continue;
}

static void codegen_block(Ast *block)
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
                codegen_block(stmt);
                break;
            case AST_WHILE_STMT:
                codegen_while(stmt);
                break;
            case AST_IF_STMT:
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

