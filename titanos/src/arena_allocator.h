#pragma once


void init_arena(void);
void *malloc_arena(unsigned long mem);
void free_arena(void);

void run_arena_allocator_tests(void);
