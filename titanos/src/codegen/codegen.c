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

#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include "llvm-c/Transforms/Utils.h"

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
__thread static LLVMBasicBlockRef active_entry = NULL;

static void codegen_decl_alloca(Decl *decl)
{
    assert(decl->type_id == DECL_VAR);
    LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(active_builder);
    LLVMValueRef inst = LLVMGetFirstInstruction(active_entry);
    if (inst != NULL)
    {
        LLVMPositionBuilderBefore(active_builder, inst);
    }
    else
    {
        LLVMPositionBuilderAtEnd(active_builder, active_entry);
    }
    decl->var.llvm_ref = LLVMBuildAlloca(active_builder, llvm_type(decl->type), "");
    LLVMSetValueName2(decl->var.llvm_ref, decl->name.start, decl->name.length);
    LLVMPositionBuilderAtEnd(active_builder, insert_block);
}
static LLVMTypeRef codegen_convert_func_decl(Decl *decl)
{
    FuncDecl *func_decl = &decl->func_decl;
    LLVMTypeRef return_type = llvm_type(func_decl->rtype);
    LLVMTypeRef* params = (LLVMTypeRef *)malloc_arena(sizeof(LLVMTypeRef) * func_decl->args->size);
    for (unsigned i = 0; i < func_decl->args->size; i++)
    {
        Decl *param_decl = func_decl->args->entries[i];
        params[i] = llvm_type(param_decl->type);
    }
    return decl->type->llvm_type = LLVMFunctionType(return_type, params, func_decl->args->size, func_decl->variadic);
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
    return decl->type->llvm_type = LLVMStructType(members, struct_decl->members->size, false);
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
    assert(!decl->type->llvm_type);
    switch (decl->type_id)
    {
        case DECL_BUILTIN:
        case DECL_ENUM_CONSTANT:
            FATAL_ERROR("LLVM type should already have been generated");
        case DECL_FUNC:
            return codegen_convert_func_decl(decl);
        case DECL_VAR:
            return llvm_type(decl->type);
        case DECL_ALIAS_TYPE:
            return codegen_convert_type(decl->type);
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
        case TYPE_ENUM:
            return type->llvm_type = codegen_convert_type(type->decl->enum_decl.type);
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
        case TYPE_STRUCT:
        case TYPE_UNION:
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
    assert(type);
    if (type->llvm_type) return type->llvm_type;
    type->llvm_type = codegen_convert_type(type);
    return type->llvm_type;
}

void codegen_function_proto(LLVMModuleRef llvm_module, Parser *parser, Decl *function)
{
    __thread static char buffer[128];
    strncpy(buffer, function->name.start, MIN(function->name.length, 127));
    LLVMValueRef fun = LLVMAddFunction(llvm_module, buffer, llvm_type(function->type));
    bool single_module = false;
    bool external = (function->is_public && !single_module) || token_compare_str(&function->name, "main");
    LLVMSetLinkage(fun, external ? LLVMExternalLinkage : LLVMLinkerPrivateLinkage);
    function->func_decl.llvm_function_proto = fun;
}

LLVMValueRef codegen_const_expr(Expr *expr)
{
    assert(expr->expr_id == EXPR_CONST);
    LLVMTypeRef type = llvm_type(expr->type);
    Value *value = &expr->const_expr.value;
    switch (value->type)
    {
        case VALUE_TYPE_FLOAT:
            if (value->float_bits <= sizeof(double) * 8)
            {
                return LLVMConstReal(type, (double)value->f);
            }
            else
            {
                char buffer[256];
                sprintf(buffer, "%Lg", value->f);
                return LLVMConstRealOfString(type, buffer);
            }
        case VALUE_TYPE_INT:
            assert(value->int_bits && "Compile time int not allowed here");
            if (value->big_int.digit_count == 0)
            {
                return LLVMConstNull(type);
            }
            else
            {
                LLVMValueRef unsigned_val;
                if (value->big_int.digit_count == 1)
                {
                    unsigned_val = LLVMConstInt(type, value->big_int.digit, false);
                }
                else
                {
                    unsigned_val = LLVMConstIntOfArbitraryPrecision(type, value->big_int.digit_count, value->big_int.digits);
                }
                return value->big_int.is_negative ? LLVMConstNeg(unsigned_val) : unsigned_val;
            }
        case VALUE_TYPE_BOOL:
            return LLVMConstInt(type, value->b ? 1 : 0, false);
        case VALUE_TYPE_NIL:
            return LLVMConstNull(type);
        case VALUE_TYPE_STRING:
            return LLVMConstStringInContext(active_context, value->str, value->str_len, false);
        case VALUE_TYPE_ERROR:
            UNREACHABLE
    }
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
    return expr->type->type_id == TYPE_FLOAT || expr->type->type_id == TYPE_CONST_FLOAT;
}

static inline bool is_signed(Expr *expr)
{
    return expr->type->type_id == TYPE_INT || expr->type->type_id == TYPE_CONST_INT;
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
            if (target_type->float_bits > source_type->float_bits)
            {
                return LLVMBuildFPExt(active_builder, value, type, "");
            }
            if (target_type->float_bits < source_type->float_bits)
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
            if (target_type->integer.bits > source_type->integer.bits)
            {
                return LLVMBuildZExt(active_builder, value, type, "");
            }
            if (target_type->integer.bits < source_type->integer.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            assert(false && "UI cast to same type");
            return value;
        case CAST_UISI:
            if (target_type->integer.bits > source_type->integer.bits)
            {
                return LLVMBuildZExt(active_builder, value, type, "");
            }
            if (target_type->integer.bits < source_type->integer.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            // If same bit size, this is a noop, since LLVM ignores sign.
            return value;
        case CAST_SIFP:
            return LLVMBuildSIToFP(active_builder, value, type, "");
        case CAST_SISI:
            if (target_type->integer.bits > source_type->integer.bits)
            {
                return LLVMBuildSExt(active_builder, value, type, "");
            }
            if (target_type->integer.bits < source_type->integer.bits)
            {
                return LLVMBuildTrunc(active_builder, value, type, "");
            }
            assert(false && "SI cast to same type");
            return value;
        case CAST_SIUI:
            if (target_type->integer.bits > source_type->integer.bits)
            {
                return LLVMBuildSExt(active_builder, value, type, "");
            }
            if (target_type->integer.bits > source_type->integer.bits)
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

static LLVMValueRef codegen_cond(Ast *ast)
{
    switch (ast->cond_stmt.cond_type)
    {
        case COND_EXPR:
            return codegen_expr(ast->cond_stmt.expr);
        case COND_DECL:
            TODO;
    }

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
            return expr->identifier_expr.is_ref ? expr->identifier_expr.resolved->var.llvm_ref : LLVMBuildLoad(active_builder, expr->identifier_expr.resolved->var.llvm_ref, "");
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
    codegen_decl_alloca(decl);
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

static inline void codegen_if(Ast *stmt)
{
    assert(stmt->ast_id == AST_IF_STMT);

    LLVMValueRef cond_value = codegen_cond(stmt->if_stmt.cond);

    // Do shortcut on constant values.
    switch (ast_cond_value(stmt->if_stmt.cond))
    {
        case COND_VARIABLE:
            break;
        case COND_TRUE:
            codegen_block(stmt->if_stmt.then_body);
            return;
        case COND_FALSE:
            if (stmt->if_stmt.else_body)
            {
                codegen_block(stmt->if_stmt.else_body);
            }
            return;
    }

    LLVMBasicBlockRef then_block = codegen_insert_block("then");
    LLVMBasicBlockRef else_block = stmt->if_stmt.else_body ? codegen_insert_block("else") : NULL;

    LLVMBasicBlockRef post_block = NULL;

    if (stmt->exit != EXIT_NONE)
    {
        post_block = codegen_insert_block("if_post");
    }

    LLVMBuildCondBr(active_builder, cond_value, then_block, else_block ?: post_block);

    // Left branch.
    LLVMPositionBuilderAtEnd(active_builder, then_block);

    codegen_block(stmt->if_stmt.then_body);

    if (stmt->if_stmt.then_body->exit != EXIT_NONE)
    {
        LLVMBuildBr(active_builder, post_block);
    }

    if (stmt->if_stmt.else_body)
    {
        // Right branch.
        LLVMMoveBasicBlockAfter(else_block, LLVMGetInsertBlock(active_builder));
        LLVMPositionBuilderAtEnd(active_builder, else_block);
        codegen_block(stmt->if_stmt.else_body);
        if (stmt->if_stmt.else_body->exit != EXIT_NONE)
        {
            LLVMBuildBr(active_builder, post_block);
        }
    }
    if (stmt->exit != EXIT_NONE)
    {
        // Continue in the post block.
        LLVMMoveBasicBlockAfter(post_block, LLVMGetInsertBlock(active_builder));
        LLVMPositionBuilderAtEnd(active_builder, post_block);
    }
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
    LLVMBuildCondBr(active_builder, codegen_cond(stmt->while_stmt.cond), while_block, while_end);
    LLVMPositionBuilderAtEnd(active_builder, while_block);
    codegen_block(stmt->while_stmt.body);

    LLVMBuildBr(active_builder, while_start);
    LLVMPositionBuilderAtEnd(active_builder, while_end);

    active_break = prev_break;
    active_continue = prev_continue;
}

static inline void codegen_do(Ast *stmt)
{
    // Store break and continue statements
    LLVMBasicBlockRef prev_break = active_break;
    LLVMBasicBlockRef prev_continue = active_continue;



    assert(stmt->exit == EXIT_RETURN || stmt->exit == EXIT_NONE);

    LLVMBasicBlockRef do_end = active_break = stmt->exit == EXIT_RETURN ? NULL : codegen_insert_block("do.end");

    LLVMBasicBlockRef do_cond = active_continue = do_end && !expr_is_const_false(stmt->expr_stmt.expr)
            ? codegen_insert_block("do.cond") : do_end;

    LLVMBasicBlockRef do_body = codegen_insert_block("do.body");

    LLVMBuildBr(active_builder, do_body);
    LLVMPositionBuilderAtEnd(active_builder, do_body);
    codegen_block(stmt->do_stmt.body);

    if (do_end)
    {
        // Skip branch to end if it ends with break or continue.
        if (stmt->do_stmt.body->exit == EXIT_NONE) LLVMBuildBr(active_builder, do_cond);

        // Emit cond if we have one
        if (do_cond != do_end)
        {
            LLVMPositionBuilderAtEnd(active_builder, do_cond);
            LLVMBuildCondBr(active_builder, codegen_expr(stmt->do_stmt.expr), do_body, do_end);
        }

        // Position to end
        LLVMPositionBuilderAtEnd(active_builder, do_end);
    }

    active_break = prev_break;
    active_continue = prev_continue;
}

static inline void codegen_break(Ast *stmt)
{
    // Emit defers
    LLVMBuildBr(active_builder, active_break);
}

static inline void codegen_continue(Ast *stmt)
{
    // Emit defers
    LLVMBuildBr(active_builder, active_continue);
}

static inline void codegen_return(Ast *stmt)
{
    if (stmt->return_stmt.expr)
    {
        LLVMValueRef retval = codegen_expr(stmt->return_stmt.expr);
        // Release defers
        LLVMBuildRet(active_builder, retval);
    }
    else
    {
        // Release defers
        LLVMBuildRetVoid(active_builder);
    }
}

static void codegen_block(Ast *block)
{
    LOG_FUNC

    assert(block->ast_id == AST_COMPOUND_STMT);
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
            case AST_BREAK_STMT:
                codegen_break(stmt);
                break;
            case AST_CONTINUE_STMT:
                codegen_continue(stmt);
                break;
            case AST_RETURN_STMT:
                codegen_return(stmt);
                break;
            case AST_IF_STMT:
                codegen_if(stmt);
                break;
            case AST_DO_STMT:
                codegen_do(stmt);
                break;
            case AST_DEFER_STMT:
            case AST_SWITCH_STMT:
            case AST_CASE_STMT:
            case AST_DEFAULT_STMT:
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
void codegen_parameter_var(Decl *param, unsigned index)
{
    assert(param->type_id == DECL_VAR && param->var.kind == VARDECL_PARAM);
    codegen_decl_alloca(param);
    LLVMBuildStore(active_builder, LLVMGetParam(active_func->func_decl.llvm_function_proto, index), param->var.llvm_ref);
}

void codegen_function(Decl *function)
{
    LOG_FUNC

    assert(!active_func);
    LLVMBuilderRef previous_builder = active_builder;
    LLVMBasicBlockRef previous_entry = active_entry;

    active_func = function;
    active_break = NULL;
    active_continue = NULL;

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(active_context, active_func->func_decl.llvm_function_proto, "entry");

    active_builder = LLVMCreateBuilderInContext(active_context);
    active_entry = entry;
    LLVMPositionBuilderAtEnd(active_builder, entry);

    // Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
    for (unsigned i = 0; i < function->func_decl.args->size; i++)
    {
        codegen_parameter_var(function->func_decl.args->entries[i], i);
    }

    codegen_block(function->func_decl.body);

    LLVMDisposeBuilder(active_builder);

    active_builder = previous_builder;

    active_func = NULL;

    active_entry = previous_entry;
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
                codegen_function(function);
            }
        }
    }


    // Verify generated IR
    char *error = NULL;
    LLVMDumpModule(llvm_module);
    bool failed = LLVMVerifyModule(llvm_module, LLVMReturnStatusAction, &error);
    if (failed)
    {
        printf("Crap: %s\n", error);
      //  exit(-1);
    }

    LLVMPassManagerRef passmgr = LLVMCreatePassManager();
    LLVMAddDemoteMemoryToRegisterPass(passmgr);        // Demote allocas to registers.
    LLVMAddInstructionCombiningPass(passmgr);        // Do simple "peephole" and bit-twiddling optimizations
    LLVMAddReassociatePass(passmgr);                // Reassociate expressions.
    LLVMAddGVNPass(passmgr);                        // Eliminate common subexpressions.
    LLVMAddCFGSimplificationPass(passmgr);            // Simplify the control flow graph
    //LLVMAddFunctionInliningPass(passmgr);        // Function inlining
    LLVMRunPassManager(passmgr, llvm_module);
    LLVMDisposePassManager(passmgr);

    LLVMDumpModule(llvm_module);
}

