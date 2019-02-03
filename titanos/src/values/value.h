#pragma once
//
// Created by Christoffer Lernö on 2019-01-25.
//
#include "common.h"
#include "bigint.h"

typedef enum _ValueType
{
    TYPE_FLOAT,
    TYPE_INT,
    TYPE_BOOL,
    TYPE_NIL,
    TYPE_STRING,
    TYPE_ERROR,
} ValueType;

typedef struct _BigInt BigInt;

typedef struct _Value
{
    ValueType type;
    union
    {
        BigInt big_int;
        long double f;
        bool b;
        struct {
            const char *str;
            uint32_t str_len;
        };
    };
} Value;

bool value_is_number(const Value *value);
Value value_new_int_with_bigint(BigInt big_int);
Value value_new_int_with_int(int64_t val);
Value value_new_float(long double f);
Value value_new_string(const char *string, uint32_t len);
Value value_new_bool(bool value);
Value value_to_bool(Value value);
Value value_negate(Value value);
Value value_mult(Value lhs, Value rhs);
Value value_sub(Value lhs, Value rhs);
Value value_add(Value lhs, Value rhs);
Value value_mod(Value lhs, Value rhs);
Value value_div(Value lhs, Value rhs);
Value value_not(Value value);
Value value_nil();
const char *value_type_name(Value value);
void value_print(Value value);
bool value_convert_to_type(Value *value, ValueType type, const char *type_name);

