#pragma once
//
// Created by Christoffer Lernö on 2019-01-25.
//
#include "common.h"

typedef enum _ValueType
{
    TYPE_FLOAT,
    TYPE_UINT,
    TYPE_INT,
    TYPE_BOOL,
    TYPE_NIL,
    TYPE_STRING,
    TYPE_ERROR,
} ValueType;

typedef struct _Value
{
    void *val;
    ValueType type;
} Value;

Value value_new_int(const char *string, uint32_t len, uint8_t radix);
Value value_new_int_with_uint(uint64_t val);
Value value_new_int_with_int(int64_t val);
Value value_new_float(const char *string, uint32_t len);
Value value_new_string(const char *string, uint32_t len);
Value value_new_bool(bool value);
Value value_to_bool(Value value);
Value value_negate(Value value);
Value value_mult(Value lhs, Value rhs);
Value value_sub(Value lhs, Value rhs);
bool value_bool_value(Value value);
Value value_not(Value value);
Value value_nil();
const char *value_type_name(Value value);
void value_print(Value value);
bool value_convert_to_type(Value *value, ValueType type, const char *type_name);

