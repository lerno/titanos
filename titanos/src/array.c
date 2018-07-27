//
// Created by Christoffer Lerno on 2018-07-23.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <string.h>
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

void array_free(Array *array)
{
    FREE_ARRAY(void *, array->entries, array->capacity);
    array_init(array);
}

void array_free_recursive(Array *array)
{
    for (uint32_t i = 0; i < array->count; i++)
    {
        free(array->entries[i]);
    }
    array_free(array);
}

void array_init(Array *array)
{
    array->capacity = 0;
    array->count = 0;
    array->entries = NULL;
}
void array_add(Array *array, void *entry)
{
    if (array->capacity < array->count + 1)
    {
        uint32_t old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(old_capacity);
        array->entries = GROW_ARRAY(array->entries, void *, old_capacity, array->capacity);
    }
    array->entries[array->count++] = entry;
}

bool array_contains_string(Array *string_list, const char *key)
{
    for (unsigned i = 0; i < string_list->count; i++)
    {
        if (strcmp(key, string_list->entries[i]) == 0) return true;
    }
    return false;
}
