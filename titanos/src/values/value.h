#pragma once
//
// Created by Christoffer Lernö on 2019-01-25.
//
#include "common.h"
#include "bigint.h"

// DO NOT CHANGE ORDER!
typedef enum _ValueType
{
    VALUE_TYPE_FLOAT,
    VALUE_TYPE_INT,
    VALUE_TYPE_BOOL,
    VALUE_TYPE_NIL,
    VALUE_TYPE_STRING,
    VALUE_TYPE_ERROR,
} ValueType;

typedef struct _BigInt BigInt;

typedef struct _Value
{
    ValueType type;
    union
    {
        struct
        {
            BigInt big_int;
            bool is_unsigned;
            uint16_t int_bits;
        };
        struct
        {
            long double f;
            uint16_t float_bits;
        };
        bool b;
        struct {
            const char *str;
            uint32_t str_len;
        };
    };
    uint32_t len;
} Value;


bool value_is_number(const Value *value);
Value value_new_int_with_bigint(BigInt big_int);
Value value_new_int_with_int(int64_t val);
Value value_new_float(long double f);
Value value_new_string(const char *string, uint32_t len);
Value value_new_bool(bool value);
Value value_to_bool(Value value);
Value value_negate(Value value);
Value value_bit_not(Value value);
Value value_mult(Value lhs, Value rhs);
Value value_sub(Value lhs, Value rhs);
Value value_add(Value lhs, Value rhs);
Value value_mod(Value lhs, Value rhs);
Value value_div(Value lhs, Value rhs);
Value value_not(Value value);
Value value_nil();
void value_update_to_float(Value *value, long double f, uint16_t bits);
const char *value_type_name(const Value *value);
typedef struct _Type Type;
Type *value_find_type(const Value *value);
void value_print(Value value);
bool value_as_bool(Value *value);

bool value_convert_to_type(Value *value1, Value *value2);
bool value_convert(Value *value, ValueType type, uint16_t bits, bool is_unsigned, bool allow_trunc);

