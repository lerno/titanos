#pragma once

#include "common.h"

// Assume well-formed hex!
static inline uint64_t parse_uint64_hex(const char *string, int len)
{
	uint64_t value = 0;
	const char *end = string + len;
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		if (c < 'A')
		{
			value = (value << 4) + (c - '0');
		}
		else if (c < 'a')
		{
			value = (value << 4) + (c - 'A' + 10);
		}
		else
		{
			value = (value << 4) + (c - 'a' + 10);
		}
	}
	return value;
}

static inline uint64_t parse_uint64_oct(const char *string, int len)
{
	uint64_t value = 0;
	const char *end = string + len;
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		value = (value << 3) + (c - '0');
	}
	return value;
}

static inline uint64_t parse_uint64_bin(const char *string, int len)
{
	uint64_t value = 0;
	const char *end = string + len;
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		value = (value << 1) + (c - '0');
	}
	return value;
}

// Parse normal integers, parse 0xBEEF, parse 0o1337, parse 0b1010101 – positive numbers only
static inline uint64_t parse_uint64(const char *string, int len)
{
	if (len > 2)
	{
		switch (string[1])
		{
			case 'x':
				return parse_uint64_hex(string + 2, len - 2);
			case 'o':
				return parse_uint64_oct(string + 2, len - 2);
			case 'b':
				return parse_uint64_bin(string + 2, len - 2);
			default:
				break;
		}
	}
	uint64_t value = 0;
	const char *end = string + len;
	while (string < end)
	{
		char c = *(string++);
		if (c == '_') continue;
		value = value * 10 + c - '0';
	}
	return value;
}
