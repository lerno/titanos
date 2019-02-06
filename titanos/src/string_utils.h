#pragma once

#include <memory.h>
#include "value.h"
#include "common.h"
#include "stdlib.h"

// Assume well-formed hex!
static inline Value parse_hex(const char *string, int len)
{
	Value value = { .type = VALUE_TYPE_INT };
	BigInt *b = &value.big_int;
	bigint_init_signed(b, 0);
	const char *end = string + len;
	BigInt temp = { .digit_count = 0 };
	BigInt add = { .digit_count = 0 };
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		bigint_shl_int(&temp, b, 4);
		if (c < 'A')
		{
			bigint_init_signed(&add, (c - '0'));
		}
		else if (c < 'a')
		{
			bigint_init_signed(&add, (c - 'A' + 10));
		}
		else
		{
			bigint_init_signed(&add, (c - 'a' + 10));
		}
		bigint_add(b, &temp, &add);
	}
	return value;
}

static inline Value parse_dec(const char *string, int len)
{
	Value value = { .type = VALUE_TYPE_INT };
	BigInt *b = &value.big_int;
	bigint_init_signed(b, 0);
	const char *end = string + len;
	BigInt temp = { .digit_count = 0 };
	BigInt mult;
	bigint_init_signed(&mult, 10);
	BigInt add = { .digit_count = 0 };
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		bigint_mul(&temp, b, &mult);
		bigint_init_signed(&add, (c - '0'));
		bigint_add(b, &temp, &add);
	}
	return value;
}

static inline Value parse_oct(const char *string, int len)
{
	Value value = { .type = VALUE_TYPE_INT };
	BigInt *b = &value.big_int;
	bigint_init_signed(b, 0);
	const char *end = string + len;
	BigInt temp = { .digit_count = 0 };
	BigInt add = { .digit_count = 0 };
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		bigint_shl_int(&temp, b, 3);
		bigint_init_signed(&add, (c - '0'));
		bigint_add(b, &temp, &add);
	}
	return value;
}

static inline Value parse_bin(const char *string, int len)
{
	Value value = { .type = VALUE_TYPE_INT };
	BigInt *b = &value.big_int;
	bigint_init_signed(b, 0);
	const char *end = string + len;
	BigInt temp = { .digit_count = 0 };
	BigInt add = { .digit_count = 0 };
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		bigint_shl_int(&temp, b, 1);
		bigint_init_signed(&add, (c - '0'));
		bigint_add(b, &temp, &add);
	}
	return value;
}

static inline char *path_to_underscore_prefix(char *namespace_name)
{
	char *result = malloc(strlen(namespace_name) + 2);
	char *result_iterator = result;
	// Create name with underscore;
	do
	{
		if (*namespace_name == ':')
		{
			namespace_name += 2;
			*(result_iterator++) = '_';
			continue;
		}
		*(result_iterator++) = *(namespace_name++);
	} while (*namespace_name);
	*(result_iterator++) = '_';
	*(result_iterator++) = 0;
	return result;
}

// Parse normal integers, parse 0xBEEF, parse 0o1337, parse 0b1010101 – positive numbers only
static inline Value parse_int(const char *string, int len)
{
	if (len > 2)
	{
		switch (string[1])
		{
			case 'x':
				return parse_hex(string + 2, (uint16_t) (len - 2));
			case 'o':
				return parse_oct(string + 2, (uint16_t) (len - 2));
			case 'b':
				return parse_bin(string + 2, (uint16_t) (len - 2));
			default:
				break;
		}
	}
	return parse_dec(string, (uint16_t) len);
}

