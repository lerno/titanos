//
// Created by Christoffer Lerno on 2018-07-29.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <memory.h>
#include "scratch_buffer.h"

#define SCRATCH_BUFFER_SIZE 16384

char scratch_buffer[SCRATCH_BUFFER_SIZE];
int scratch_buffer_pointer;

void scratch_buffer_clear(void)
{
	scratch_buffer_pointer = 0;
}

char *scratch_buffer_start(void)
{
	return scratch_buffer;
}

int scratch_buffer_length(void)
{
	return scratch_buffer_pointer;
}

void write_to_scratch_length(const char *string, int length)
{
	memcpy(scratch_buffer, string, length);
}
void write_to_scratch(const char *string)
{
	write_to_scratch_length(string, (int)strlen(string));
}
