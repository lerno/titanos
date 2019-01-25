//
// Created by Christoffer Lernö on 2019-01-25.
//

#include <llvm-c/Types.h>
#include <llvm-c/Core.h>
#include <error.h>
#include "value.h"

#define ERROR_VALUE (Value) { NULL, TYPE_ERROR }

LLVMTypeRef constFP()
{
    return LLVMFP128Type();
}

LLVMTypeRef constInt()
{
    return LLVMInt128Type();
}

static LLVMValueRef zero_fp()
{
    static LLVMValueRef zero = NULL;
    if (!zero)
    {
        zero = LLVMConstReal(constFP(), 0.0);
    }
    return zero;
}

static LLVMValueRef zero_uint()
{
    static LLVMValueRef zero = NULL;
    if (!zero)
    {
        zero = LLVMConstInt(constInt(), 0, false);
    }
    return zero;
}

static LLVMValueRef zero_int()
{
    static LLVMValueRef zero = NULL;
    if (!zero)
    {
        zero = LLVMConstInt(constInt(), 0, true);
    }
    return zero;
}

Value value_new_int(const char *string, uint32_t len, uint8_t radix)
{
    return (Value) { LLVMConstIntOfStringAndSize(LLVMIntType(128), string, len, radix), TYPE_INT };
}

Value value_new_int_with_uint(uint64_t val)
{
    return (Value) { LLVMConstInt(LLVMInt64Type(), val, false), TYPE_UINT };
}

Value value_new_int_with_int(int64_t val)
{
    return (Value) { LLVMConstInt(LLVMInt64Type(), (uint64_t)val, true), TYPE_INT };
}

Value value_new_float(const char *string, uint32_t len)
{
    if (len == 1 && string[0] == '0') return (Value) { zero_fp(), TYPE_FLOAT };
    return (Value) { LLVMConstRealOfStringAndSize(LLVMIntType(128), string, len), TYPE_FLOAT };
}

Value value_nil()
{
    return (Value) { NULL, TYPE_NIL };
}


Value value_not(Value value)
{
    switch (value.type)
    {
        case TYPE_FLOAT:
            return (Value) { LLVMConstFCmp(LLVMRealOEQ, value.val, zero_fp()), TYPE_BOOL };
        case TYPE_UINT:
        case TYPE_INT:
            return (Value) { LLVMConstICmp(LLVMIntEQ, value.val, zero_int()), TYPE_BOOL };
        case TYPE_BOOL:
            return (Value) { LLVMConstNot(value.val), TYPE_BOOL };
        case TYPE_NIL:
            return value_new_bool(true);
        case TYPE_STRING:
        case TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

bool value_bool_value(Value value)
{
    assert(value.type == TYPE_BOOL);
    return LLVMConstIntGetZExtValue(value.val) != 0;
}


Value value_mult(Value lhs, Value rhs)
{
    assert(lhs.type == rhs.type);
    switch (lhs.type)
    {
        case TYPE_FLOAT:
            return (Value) { LLVMConstFMul(lhs.val, rhs.val), TYPE_FLOAT };
        case TYPE_INT:
        case TYPE_UINT:
            return (Value) { LLVMConstMul(lhs.val, rhs.val), lhs.type };
        case TYPE_BOOL:
            return (Value) { LLVMConstMul(LLVMConstZExt(lhs.val, constInt()), LLVMConstZExt(lhs.val, constInt())), TYPE_UINT };
        case TYPE_STRING:
        case TYPE_NIL:
        case TYPE_ERROR:
            return ERROR_VALUE;
    }
}

Value value_sub(Value lhs, Value rhs)
{
    assert(lhs.type == rhs.type);
    switch (lhs.type)
    {
        case TYPE_FLOAT:
            return (Value) { LLVMConstFSub(lhs.val, rhs.val), TYPE_FLOAT };
        case TYPE_INT:
        case TYPE_UINT:
            return (Value) { LLVMConstSub(lhs.val, rhs.val), lhs.type };
        case TYPE_BOOL:
            return (Value) { LLVMConstSub(LLVMConstZExt(lhs.val, constInt()), LLVMConstZExt(lhs.val, constInt())), TYPE_UINT };
        case TYPE_STRING:
        case TYPE_NIL:
        case TYPE_ERROR:
            return ERROR_VALUE;
    }
}


Value value_negate(Value value)
{
    switch (value.type)
    {
        case TYPE_UINT:
        case TYPE_INT:
            return (Value) { LLVMConstNeg(value.val), TYPE_INT };
        case TYPE_BOOL:
            return (Value) { LLVMConstNeg(LLVMConstSExt(value.val, LLVMInt128Type())), TYPE_INT };
        case TYPE_FLOAT:
            return (Value) { LLVMConstFNeg(value.val), TYPE_FLOAT };
        case TYPE_NIL:
        case TYPE_STRING:
        case TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

Value value_new_bool(bool value)
{
    static LLVMTypeRef t = NULL;
    static LLVMTypeRef f = NULL;
    if (!value)
    {
        if (!f) f = LLVMConstInt(LLVMInt1Type(), 0, false);
        return (Value) { f, TYPE_BOOL };
    }
    else
    {
        if (!t) t = LLVMConstInt(LLVMInt1Type(), 1, false);
        return (Value) { t, TYPE_BOOL };
    }
}

Value value_new_string(const char *string, uint32_t len)
{
    return (Value) { LLVMConstString(string, len, false), TYPE_STRING };
}

Value value_to_bool(Value value)
{
    switch (value.type)
    {
        case TYPE_FLOAT:
            return (Value) { LLVMConstFCmp(LLVMRealONE, value.val, zero_fp()), TYPE_BOOL };
        case TYPE_UINT:
        case TYPE_INT:
            return (Value) { LLVMConstICmp(LLVMIntNE, value.val, zero_int()), TYPE_BOOL };
        case TYPE_BOOL:
            return value;
        case TYPE_NIL:
            return value_new_bool(false);
        case TYPE_STRING:
        case TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

const char *value_type_name(Value value)
{
    switch (value.type)
    {
        case TYPE_BOOL:
            return "bool";
        case TYPE_NIL:
            return "nil";
        case TYPE_FLOAT:
            return "float";
        case TYPE_STRING:
            return "string";
        case TYPE_UINT:
        case TYPE_INT:
            return "int";
        case TYPE_ERROR:
            return "<error>";
    }
    FATAL_ERROR("Can't happen");
}


void value_print(Value value)
{
    char *c = LLVMPrintValueToString(value.val);
    printf("%s", c);
    LLVMDisposeMessage(c);
}


bool value_convert_to_type(Value *value, ValueType type, const char *type_name)
{
    if (type == value->type) return true;
    switch (value->type)
    {
        case TYPE_STRING:
            return false;
        case TYPE_FLOAT:
            FATAL_ERROR("Conversion should never happen");
        case TYPE_INT:
            assert(type == TYPE_FLOAT);
            *value = (Value) { LLVMConstSIToFP(value->val, constFP()), TYPE_FLOAT };
            return true;
        case TYPE_UINT:
            if (type == TYPE_UINT)
            {
                *value = (Value) { value->val, TYPE_INT };
                return true;
            }
            assert(type == TYPE_FLOAT);
            *value = (Value) { LLVMConstUIToFP(value->val, constFP()), TYPE_FLOAT };
            return true;
        case TYPE_BOOL:
            if (type == TYPE_FLOAT)
            {
                *value = (Value) { LLVMConstUIToFP(value->val, constFP()), TYPE_FLOAT };
                return true;
            }
            if (type == TYPE_INT)
            {
                *value = (Value) { LLVMConstIntCast(value->val, constInt(), true), TYPE_INT };
                return true;
            }
            if (type == TYPE_UINT)
            {
                *value = (Value) { LLVMConstIntCast(value->val, constInt(), false), TYPE_UINT };
                return true;
            }
            FATAL_ERROR("Should never happen");
            return false;
        case TYPE_NIL:
            if (type == TYPE_FLOAT)
            {
                *value = (Value) { zero_fp(), TYPE_FLOAT };
                return true;
            }
            if (type == TYPE_INT)
            {
                *value = (Value) { zero_int(), TYPE_INT };
                return true;
            }
            if (type == TYPE_UINT)
            {
                *value = (Value) { zero_uint(), TYPE_UINT };
                return true;
            }
            if (type == TYPE_BOOL)
            {
                *value = value_new_bool(false);
                return true;
            }
            FATAL_ERROR("Should never happen");
            return false;
        case TYPE_ERROR:
            break;
    }
    FATAL_ERROR("Should never happen");
}
