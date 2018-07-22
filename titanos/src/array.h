#pragma once
//
// Created by Christoffer Lerno on 2018-07-23.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdlib.h>
#include "common.h"

#define GROW_CAPACITY(c) ((c) < 8 ? 8 : (c) * 2)

#define GROW_ARRAY(previous, type, old_size, new_size) \
	(type *)reallocate(previous, sizeof(type) * old_size, sizeof(type) * new_size)

#define FREE_ARRAY(type, pointer, old_count) \
	reallocate(pointer, sizeof(type) * (old_count), 0)

void *reallocate(void *previous, size_t old_size, size_t new_size);


typedef struct _Array
{
    uint32_t count;
    uint32_t capacity;
    void **entries;
} Array;

void array_init(Array *list);
void array_free(Array *list);
void array_free_recursive(Array *array); // Frees stored elements as well
void array_add(Array *list, void *entry);
bool array_contains_string(Array *string_list, const char *key);
