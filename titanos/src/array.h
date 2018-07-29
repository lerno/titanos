#pragma once
//
// Created by Christoffer Lerno on 2018-07-23.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdlib.h>

#define GROW_CAPACITY(c) ((c) < 8 ? 8 : (c) * 2)

#define GROW_ARRAY(previous, type, old_size, new_size) \
	(type *)reallocate(previous, sizeof(type) * old_size, sizeof(type) * new_size)

#define FREE_ARRAY(type, pointer, old_count) \
	reallocate(pointer, sizeof(type) * (old_count), 0)

void *reallocate(void *previous, size_t old_size, size_t new_size);
