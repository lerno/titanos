//
// Created by Christoffer Lerno on 2018-07-23.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "array.h"

void *reallocate(void *previous, size_t old_size, size_t new_size)
{
	if (new_size == 0)
	{
		free(previous);
		return NULL;
	}

	if (new_size == old_size) return previous;

	return realloc(previous, new_size);

}
