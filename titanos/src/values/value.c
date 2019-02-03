//
// Created by Christoffer Lernö on 2019-01-25.
//

#include <llvm-c/Types.h>
#include <llvm-c/Core.h>
#include <error.h>
#include "value.h"

#define ERROR_VALUE (Value) { .type = TYPE_ERROR }


Value value_new_int_with_bigint(BigInt big_int)
{
    return (Value) { .big_int = big_int, .type = TYPE_INT };
}

Value value_new_float(long double f)
{
    return (Value) { .f = f, .type = TYPE_INT };
}
Value value_new_int_with_int(int64_t val)
{
    Value value;
    bigint_init_signed(&value.big_int, val);
    value.type = TYPE_INT;
    return value;
}


Value value_nil()
{
    return (Value) { .b = false, .type = TYPE_NIL };
}


Value value_not(Value value)
{
    switch (value.type)
    {
        case TYPE_FLOAT:
            return (Value) { .b = value.f == 0.0, .type = TYPE_BOOL };
        case TYPE_INT:
            return (Value) { .b = bigint_cmp_zero(&value.big_int) == INT_EQ, .type = TYPE_BOOL };
        case TYPE_BOOL:
            return (Value) { .b = !value.b, .type = TYPE_BOOL };
        case TYPE_NIL:
            return value_new_bool(true);
        case TYPE_STRING:
        case TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}


#define BIN_OP(_x, _intop, _floatop) \
Value value_## _x(Value lhs, Value rhs) { \
    assert(lhs.type == rhs.type); \
    switch (lhs.type)\
    { \
        case TYPE_FLOAT: \
            return (Value) { .f = lhs.f _floatop rhs.f, .type = TYPE_FLOAT }; \
        case TYPE_INT: {\
            Value value = value_new_int_with_int(0);\
            _intop(&value.big_int, &lhs.big_int, &rhs.big_int);\
            return value;\
        }\
        case TYPE_BOOL:\
        case TYPE_STRING:\
        case TYPE_NIL:\
        case TYPE_ERROR:\
            return ERROR_VALUE;\
    }\
}

BIN_OP(sub, bigint_sub, -)
BIN_OP(add, bigint_add, +)
BIN_OP(mult, bigint_mul, *)
BIN_OP(div, bigint_div_floor, /)
BIN_OP(mod, bigint_mod, /)

Value value_negate(Value value)
{
    switch (value.type)
    {
        case TYPE_INT:
        {
            Value result = value_new_int_with_int(0);
            bigint_negate(&result.big_int, &value.big_int);
            return result;
        }
        case TYPE_BOOL:
            return value_new_int_with_int(value.b ? -1 : 0);
        case TYPE_FLOAT:
            return value_new_float(-value.f);
        case TYPE_NIL:
        case TYPE_STRING:
        case TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

inline Value value_new_bool(bool value)
{
    return (Value) { .b = value, .type = TYPE_BOOL };
}

Value value_new_string(const char *string, uint32_t len)
{
    return (Value) { .str = string, .str_len = len, .type = TYPE_STRING };
}

Value value_to_bool(Value value)
{
    switch (value.type)
    {
        case TYPE_FLOAT:
            return value_new_bool(value.f != 0.0);
        case TYPE_INT:
            return value_new_bool(bigint_cmp_zero(&value.big_int) != INT_EQ);
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
        case TYPE_INT:
            return "int";
        case TYPE_ERROR:
            return "<error>";
    }
    FATAL_ERROR("Can't happen");
}



bool value_is_number(const Value *value)
{
    return value->type == TYPE_INT || value->type == TYPE_FLOAT;
}


void value_print(Value value)
{
    switch (value.type)
    {
        case TYPE_BOOL:
            printf(value.b ? "true" : "false");
            break;
        case TYPE_STRING:
            printf("%.*s", value.str_len, value.str);
            break;
        case TYPE_INT:
            bigint_print(&value.big_int, 10);
            break;
        case TYPE_ERROR:
            printf("ERROR");
            break;
        case TYPE_FLOAT:
            printf("%Lf", value.f);
            break;
        case TYPE_NIL:
            printf("nil");
            break;
    }
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
            value->f = bigint_as_float(&value->big_int);
            value->type = TYPE_FLOAT;
            return true;
        case TYPE_BOOL:
            if (type == TYPE_FLOAT)
            {
                value->f = value->b ? 1.0 : 0.0;
                value->type = TYPE_FLOAT;
                return true;
            }
            if (type == TYPE_INT)
            {
                bigint_init_signed(&value->big_int, value->b ? 1 : 0);
                value->type = TYPE_INT;
                return true;
            }
            FATAL_ERROR("Should never happen");
            return false;
        case TYPE_NIL:
            if (type == TYPE_FLOAT)
            {
                value->f = 0.0;
                value->type = TYPE_FLOAT;
                return true;
            }
            if (type == TYPE_INT)
            {
                bigint_init_signed(&value->big_int, 0);
                value->type = TYPE_INT;
                return true;
            }
            if (type == TYPE_BOOL)
            {
                value->b = false;
                value->type = TYPE_BOOL;
            }
            FATAL_ERROR("Should never happen");
            return false;
        case TYPE_ERROR:
            break;
    }
    FATAL_ERROR("Should never happen");
}
