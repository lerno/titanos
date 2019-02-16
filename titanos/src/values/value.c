//
// Created by Christoffer Lernö on 2019-01-25.
//

#include <llvm-c/Types.h>
#include <llvm-c/Core.h>
#include <error.h>
#include "value.h"
#include "builtins.h"
#include "types/type.h"

#define ERROR_VALUE (Value) { .type = VALUE_TYPE_ERROR }


Value value_new_int_with_bigint(BigInt big_int)
{
    return (Value) { .big_int = big_int, .type = VALUE_TYPE_INT };
}

Value value_new_float(long double f)
{
    return (Value) { .f = f, .type = VALUE_TYPE_FLOAT };
}
Value value_new_int_with_int(int64_t val)
{
    Value value = { .type = VALUE_TYPE_INT };
    bigint_init_signed(&value.big_int, val);
    return value;
}


Value value_nil()
{
    return (Value) { .b = false, .type = VALUE_TYPE_NIL };
}

Value value_not(Value value)
{
    switch (value.type)
    {
        case VALUE_TYPE_FLOAT:
            return (Value) { .b = value.f == 0.0, .type = VALUE_TYPE_BOOL };
        case VALUE_TYPE_INT:
            return (Value) { .b = bigint_cmp_zero(&value.big_int) == INT_EQ, .type = VALUE_TYPE_BOOL };
        case VALUE_TYPE_BOOL:
            return (Value) { .b = !value.b, .type = VALUE_TYPE_BOOL };
        case VALUE_TYPE_NIL:
            return value_new_bool(true);
        case VALUE_TYPE_STRING:
        case VALUE_TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

#define BIN_OP(_x, _intop, _floatop) \
Value value_## _x(Value lhs, Value rhs) { \
    assert(lhs.type == rhs.type); \
    switch (lhs.type)\
    { \
        case VALUE_TYPE_FLOAT: \
            return (Value) { .f = lhs.f _floatop rhs.f, .type = VALUE_TYPE_FLOAT }; \
        case VALUE_TYPE_INT: {\
            Value value = value_new_int_with_int(0);\
            _intop(&value.big_int, &lhs.big_int, &rhs.big_int); \
            return value;\
        }\
        case VALUE_TYPE_BOOL:\
        case VALUE_TYPE_STRING:\
        case VALUE_TYPE_NIL:\
        case VALUE_TYPE_ERROR:\
            return ERROR_VALUE;\
    }\
}

#define BIN_OP_W(_x, _intop, _intopwrap, _floatop) \
Value value_## _x(Value lhs, Value rhs) { \
    assert(lhs.type == rhs.type); \
    switch (lhs.type)\
    { \
        case VALUE_TYPE_FLOAT: \
            return (Value) { .f = lhs.f _floatop rhs.f, .type = VALUE_TYPE_FLOAT }; \
        case VALUE_TYPE_INT: {\
            Value value = value_new_int_with_int(0);\
            if (lhs.int_bits > 0) { \
                 value.int_bits = lhs.int_bits; value.is_unsigned = lhs.is_unsigned; \
                 _intopwrap(&value.big_int, &lhs.big_int, &rhs.big_int, lhs.int_bits, !lhs.is_unsigned); \
            } else { \
                  _intop(&value.big_int, &lhs.big_int, &rhs.big_int); \
            };\
            return value;\
        }\
        case VALUE_TYPE_BOOL:\
        case VALUE_TYPE_STRING:\
        case VALUE_TYPE_NIL:\
        case VALUE_TYPE_ERROR:\
            return ERROR_VALUE;\
    }\
}

BIN_OP_W(add, bigint_add, bigint_add_wrap, +)
BIN_OP_W(mult, bigint_mul, bigint_mul_wrap, *)
BIN_OP(div, bigint_div_floor, /)
BIN_OP(mod, bigint_mod, /)

Value value_sub(Value value1, Value value2)
{
    return value_add(value1, value_negate(value2));
}

Value value_negate(Value value)
{
    switch (value.type)
    {
        case VALUE_TYPE_INT:
        {
            Value result = value_new_int_with_int(0);
            result.is_unsigned = value.is_unsigned;
            result.int_bits = value.int_bits;
            if (value.int_bits)
            {
                bigint_negate_wrap(&result.big_int, &value.big_int, value.int_bits);
            }
            else
            {
                bigint_negate(&result.big_int, &value.big_int);
            }
            return result;
        }
        case VALUE_TYPE_BOOL:
            return value_new_int_with_int(value.b ? -1 : 0);
        case VALUE_TYPE_FLOAT:
            return value_new_float(-value.f);
        case VALUE_TYPE_NIL:
        case VALUE_TYPE_STRING:
        case VALUE_TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

inline Value value_new_bool(bool value)
{
    return (Value) { .b = value, .type = VALUE_TYPE_BOOL };
}

Value value_new_string(const char *string, uint32_t len)
{
    return (Value) { .str = string, .str_len = len, .type = VALUE_TYPE_STRING };
}

Value value_to_bool(Value value)
{
    switch (value.type)
    {
        case VALUE_TYPE_FLOAT:
            return value_new_bool(value.f != 0.0);
        case VALUE_TYPE_INT:
            return value_new_bool(bigint_cmp_zero(&value.big_int) != INT_EQ);
        case VALUE_TYPE_BOOL:
            return value;
        case VALUE_TYPE_NIL:
            return value_new_bool(false);
        case VALUE_TYPE_STRING:
        case VALUE_TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

Value value_float(Value value)
{
    switch (value.type)
    {
        case VALUE_TYPE_FLOAT:
            return value_new_bool(value.f != 0.0);
        case VALUE_TYPE_INT:
            return value_new_bool(bigint_cmp_zero(&value.big_int) != INT_EQ);
        case VALUE_TYPE_BOOL:
            return value;
        case VALUE_TYPE_NIL:
            return value_new_bool(false);
        case VALUE_TYPE_STRING:
        case VALUE_TYPE_ERROR:
            break;
    }
    return ERROR_VALUE;
}

const char *value_type_name(const Value *value)
{
    switch (value->type)
    {
        case VALUE_TYPE_BOOL:
            return "bool";
        case VALUE_TYPE_NIL:
            return "nil";
        case VALUE_TYPE_FLOAT:
            switch (value->float_bits)
            {
                case 0: return "float";
                case 16: return "f16";
                case 32: return "f32";
                case 64: return "f64";
                case 128: return "f128";
                default:
                    UNREACHABLE;
            }
        case VALUE_TYPE_STRING:
            return "string";
        case VALUE_TYPE_INT:
            switch (value->int_bits)
            {
                case 0: return "int";
                case 8: return value->is_unsigned ? "u8" : "i8";
                case 16: return value->is_unsigned ? "u16" : "i16";
                case 32: return value->is_unsigned ? "u32" : "i32";
                case 64: return value->is_unsigned ? "u64" : "i64";
                default:
                    UNREACHABLE;
            }
        case VALUE_TYPE_ERROR:
            return "<error>";
    }
    FATAL_ERROR("Can't happen");
}



bool value_is_number(const Value *value)
{
    return value->type == VALUE_TYPE_INT || value->type == VALUE_TYPE_FLOAT;
}

Type *value_find_type(const Value *value)
{
    switch (value->type)
    {
        case VALUE_TYPE_FLOAT:
            switch (value->float_bits)
            {
                case 0: return type_compfloat();
                case 16: return type_builtin_f16();
                case 32: return type_builtin_f32();
                case 64: return type_builtin_f64();
                case 128: return type_builtin_f128();
                default: break;
            }
            UNREACHABLE
        case VALUE_TYPE_INT:
            switch (value->int_bits)
            {
                case 0: return type_compint();
                case 8: return value->is_unsigned ? type_builtin_u8() : type_builtin_i8();
                case 16: return value->is_unsigned ? type_builtin_u16() : type_builtin_i16();
                case 32: return value->is_unsigned ? type_builtin_u32() : type_builtin_i32();
                case 64: return value->is_unsigned ? type_builtin_u64() : type_builtin_i64();
                default: break;
            }
            UNREACHABLE
        case VALUE_TYPE_BOOL:
            return type_builtin_bool();
        case VALUE_TYPE_NIL:
            return type_nil();
        case VALUE_TYPE_STRING:
            return type_string();
        case VALUE_TYPE_ERROR:
            return type_invalid();
    }
}

void value_print(Value value)
{
    switch (value.type)
    {
        case VALUE_TYPE_BOOL:
            printf(value.b ? "true" : "false");
            break;
        case VALUE_TYPE_STRING:
            printf("%.*s", value.str_len, value.str);
            break;
        case VALUE_TYPE_INT:
            bigint_print(&value.big_int, 10);
            break;
        case VALUE_TYPE_ERROR:
            printf("ERROR");
            break;
        case VALUE_TYPE_FLOAT:
            printf("%Lf", value.f);
            break;
        case VALUE_TYPE_NIL:
            printf("nil");
            break;
    }
}


void value_update_to_float(Value *value, long double f, uint16_t bits)
{
    value->f = f;
    value->type = VALUE_TYPE_FLOAT;
    value->float_bits = bits;
}
/**
 * Convert value2 to value1 (note that we have already ordered things in conversion order.
 *
 * @param value1
 * @param value2
 * @return true if conversion worked.
 */
static bool value_convert_to_type_ordered(Value *value1, Value *value2)
{
    switch (value1->type)
    {
        case VALUE_TYPE_FLOAT:
            switch (value2->type)
            {
                case VALUE_TYPE_FLOAT:
                    value1->float_bits = value2->float_bits;
                    return true;
                case VALUE_TYPE_INT:
                    value_update_to_float(value2, bigint_as_float(&value2->big_int), value1->float_bits);
                    return true;
                case VALUE_TYPE_BOOL:
                    value_update_to_float(value2, value2->b ? 1.0 : 0.0, value1->float_bits);
                    return true;
                case VALUE_TYPE_NIL:
                    value_update_to_float(value2, 0.0, value1->float_bits);
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
            }
            UNREACHABLE
        case VALUE_TYPE_INT:
            switch (value2->type)
            {
                case VALUE_TYPE_INT:
                    // First check if we have a comptime int. If so, check that it fits.
                    if (value2->int_bits == 0)
                    {
                        if (value1->int_bits == 0) return true;
                        if (!bigint_fits_in_bits(&value2->big_int, value1->int_bits, !value1->is_unsigned)) return false;
                        BigInt res;
                        bigint_truncate(&res, &value2->big_int, value1->int_bits, !value1->is_unsigned);
                        value2->big_int = res;
                        return true;
                    }
                    if (!value1->is_unsigned && value2->is_unsigned)
                    {
                        // If unsigned value is same or larger, disallow!
                        if (value1->int_bits <= value2->int_bits) return false;

                        value2->is_unsigned = false;
                        value2->int_bits = value1->int_bits;
                        return true;
                    }
                    // Final case, both has same sign, promote to largest.
                    value2->int_bits = value1->int_bits;
                    return true;
                case VALUE_TYPE_BOOL:
                    bigint_init_unsigned(&value2->big_int, value2->b ? 1 : 0);
                    value2->int_bits = value1->int_bits;
                    value2->is_unsigned = value1->is_unsigned;
                    value2->type = VALUE_TYPE_INT;
                    return true;
                case VALUE_TYPE_NIL:
                    bigint_init_unsigned(&value2->big_int, 0);
                    value2->int_bits = value1->int_bits;
                    value2->is_unsigned = value1->is_unsigned;
                    value2->type = VALUE_TYPE_INT;
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
                case VALUE_TYPE_FLOAT:
                    UNREACHABLE;
            }
            UNREACHABLE;
        case VALUE_TYPE_BOOL:
            switch (value2->type)
            {
                case VALUE_TYPE_BOOL:
                    return true;
                case VALUE_TYPE_NIL:
                    value2->b = false;
                    value2->type = VALUE_TYPE_BOOL;
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
                case VALUE_TYPE_FLOAT:
                case VALUE_TYPE_INT:
                    UNREACHABLE;
            }
            UNREACHABLE;
        case VALUE_TYPE_NIL:
            switch (value2->type)
            {
                case VALUE_TYPE_NIL:
                    return true;
                case VALUE_TYPE_STRING:
                case VALUE_TYPE_ERROR:
                    return false;
                case VALUE_TYPE_FLOAT:
                case VALUE_TYPE_BOOL:
                case VALUE_TYPE_INT:
                    UNREACHABLE;
            }
            UNREACHABLE;
        case VALUE_TYPE_STRING:
            return value2->type == VALUE_TYPE_STRING;
        case VALUE_TYPE_ERROR:
            return false;
    }
    UNREACHABLE;
}

bool value_convert_to_type(Value *value1, Value *value2)
{
    bool reverse_order = false;
    if (value2->type == value1->type)
    {
        switch (value1->type)
        {
            case VALUE_TYPE_FLOAT:
                reverse_order = value2->float_bits > value1->float_bits;
                break;
            case VALUE_TYPE_INT:
                if (value1->is_unsigned != value2->is_unsigned)
                {
                    reverse_order = value1->is_unsigned;
                    break;
                }
                reverse_order = value2->int_bits > value1->int_bits;
                break;
            case VALUE_TYPE_BOOL:
            case VALUE_TYPE_NIL:
            case VALUE_TYPE_STRING:
            case VALUE_TYPE_ERROR:
                break;
        }
    }
    else
    {
        reverse_order = value2->type < value1->type;
    }
    return reverse_order ? value_convert_to_type_ordered(value2, value1) : value_convert_to_type_ordered(value1, value2);
}

static inline bool set_bits_and_truncate_int_value_if_needed(Value *value, uint16_t bits, bool allow_trunc)
{
    value->int_bits = bits;

    // No truncation
    if (bits == 0) return true;

    // If it fits then we're fine.
    if (bigint_fits_in_bits(&value->big_int, bits, !value->is_unsigned))
    {
        return true;
    }

    // If we can truncate, do so.
    if (allow_trunc)
    {
        BigInt temp;
        bigint_truncate(&temp, &value->big_int, bits, !value->is_unsigned);
        value->big_int = temp;
        return true;
    }

    // Otherwise fail.
    return false;
}

bool value_int_change_sign(Value *value, bool is_unsigned, bool allow_trunc)
{
    if (value->is_unsigned == is_unsigned) return true;
    if (value->is_unsigned)
    {
        value->is_unsigned = false;

        // No bit limit? Goodie
        if (!value->int_bits) return true;

        // If it fits, then we're golden.
        if (bigint_fits_in_bits(&value->big_int, value->int_bits, true)) return true;

        // If not and we're not allowed conversion? Exit:
        if (!allow_trunc) return false;

        BigInt temp;
        bigint_truncate(&temp, &value->big_int, value->int_bits, true);
        value->big_int = temp;
        // TODO verify that this actually works!
        return true;
    }
    else
    {
        // Signed to unsigned
        value->is_unsigned = true;

        // No bit limit? Goodie
        if (!value->int_bits) return true;

        // If the value was positive we're golden
        if (!value->big_int.is_negative) return true;

        // If not and we're not allowed conversion? Exit:
        if (!allow_trunc) return false;

        BigInt temp;
        bigint_truncate(&temp, &value->big_int, value->int_bits, false);
        value->big_int = temp;
        // TODO verify that this actually works!
        return true;
    }
}

bool value_convert(Value *value, ValueType type, uint16_t bits, bool is_unsigned, bool allow_trunc)
{
    switch (type)
    {
        case VALUE_TYPE_FLOAT:
            switch (value->type)
            {
                case VALUE_TYPE_FLOAT:
                    // TODO actual truncation
                    value->float_bits = bits;
                    break;
                case VALUE_TYPE_INT:
                    value->f = bigint_as_float(&value->big_int);
                    break;
                case VALUE_TYPE_BOOL:
                    value->f = value->b ? 1.0 : 0.0;
                    break;
                case VALUE_TYPE_NIL:
                    value->f = 0.0;
                    break;
                case VALUE_TYPE_STRING:
                    return false;
                case VALUE_TYPE_ERROR:
                    return false;
            }
            value->float_bits = bits;
            value->type = VALUE_TYPE_FLOAT;
            return true;
        case VALUE_TYPE_INT:
            switch (value->type)
            {
                case VALUE_TYPE_FLOAT:
                    if (value->f < 0 && is_unsigned)
                    {
                        if (!allow_trunc) return false;
                        // First convert to signed, then convert to unsigned.
                        bool success = value_convert(value, type, bits, false, true);
                        assert(success && "Unexpected failure");
                        return value_convert(value, type, bits, true, true);
                    }
                    // TODO actual expansion
                    bigint_init_signed(&value->big_int, (int64_t)value->f);
                    value->is_unsigned = is_unsigned;
                    value->type = VALUE_TYPE_INT;
                    return set_bits_and_truncate_int_value_if_needed(value, bits, allow_trunc);
                case VALUE_TYPE_INT:
                    if (!value_int_change_sign(value, is_unsigned, allow_trunc)) return false;
                    return set_bits_and_truncate_int_value_if_needed(value, bits, allow_trunc);
                case VALUE_TYPE_BOOL:
                    value->type = VALUE_TYPE_INT;
                    value->int_bits = bits;
                    value->is_unsigned = is_unsigned;
                    bigint_init_unsigned(&value->big_int, value->b ? 1 : 0);
                    return true;
                case VALUE_TYPE_NIL:
                    value->type = VALUE_TYPE_INT;
                    value->int_bits = bits;
                    value->is_unsigned = is_unsigned;
                    bigint_init_unsigned(&value->big_int, 0);
                    return true;
                case VALUE_TYPE_STRING:
                    return false;
                case VALUE_TYPE_ERROR:
                    return false;
            }
            UNREACHABLE
        case VALUE_TYPE_BOOL:
            switch (value->type)
            {
                case VALUE_TYPE_FLOAT:
                    if (!allow_trunc) return false;
                    value->b = value->f != 0.0;
                    break;
                case VALUE_TYPE_INT:
                    value->b = value->big_int.digit_count != 0;
                    break;
                case VALUE_TYPE_BOOL:
                    return true;
                case VALUE_TYPE_NIL:
                    value->b = false;
                    break;
                case VALUE_TYPE_STRING:
                    return false;
                case VALUE_TYPE_ERROR:
                    return false;
            }
            value->type = VALUE_TYPE_BOOL;
            return true;
        case VALUE_TYPE_NIL:
            return value->type == VALUE_TYPE_NIL;
        case VALUE_TYPE_STRING:
            return value->type == VALUE_TYPE_STRING;
        case VALUE_TYPE_ERROR:
            return false;
    }
    UNREACHABLE
}
