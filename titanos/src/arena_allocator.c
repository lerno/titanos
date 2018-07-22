#include "arena_allocator.h"

#include <stdlib.h>
#include <assert.h>
#include "error.h"

#define KB 1024L
// Use 1 MB at a time.
#define BUCKET_SIZE (1024 * KB)
#define ARENA_BUCKET_START_SIZE 16

static char **arena_buckets;
static int arena_buckets_used;
static size_t arena_buckets_array_size;
static size_t current_use;
static void *current_arena;

void init_arena(void)
{
    arena_buckets = malloc(ARENA_BUCKET_START_SIZE * sizeof(void *));
    arena_buckets_used = 1;
    arena_buckets_array_size = ARENA_BUCKET_START_SIZE;
    arena_buckets[0] = malloc(BUCKET_SIZE);
    current_use = 0;
    current_arena = arena_buckets[0];
}

// Simple bump allocator with buckets.
void *malloc_arena(size_t mem)
{
    // Round to multiple of 8
    mem = (mem + 7) & ~7;
    if (current_use + mem > BUCKET_SIZE)
    {
        if (arena_buckets_used == arena_buckets_array_size)
        {
            arena_buckets_array_size *= 2;
            arena_buckets = realloc(arena_buckets, arena_buckets_array_size * sizeof(void *));
            ASSERT(arena_buckets, "Ran out of memory after allocating %ld KB", BUCKET_SIZE * arena_buckets_used / KB);
        }
        current_arena = malloc(BUCKET_SIZE);
        ASSERT(current_arena, "Ran out of memory after allocating %ld KB", BUCKET_SIZE * arena_buckets_used / KB);
        arena_buckets[arena_buckets_used++] = current_arena;
        current_use = 0;
    }
    char *ptr = current_arena + current_use;
    current_use += mem;
    return (void *)ptr;
}


void free_arena(void)
{
    for (int i = 0; i < arena_buckets_used; i++)
    {
        free(arena_buckets[i]);
    }
    current_arena = NULL;
    arena_buckets_used = 0;
    arena_buckets = NULL;
    arena_buckets_array_size = 0;
    current_use = 0;
}


void run_arena_allocator_tests(void)
{
    init_arena();
    free_arena();
    init_arena();
    ASSERT(malloc_arena(10) != malloc_arena(10), "Expected different values...");
    ASSERT(current_use == 32, "Expected allocations rounded to next 8 bytes");
    EXPECT("buckets in use", arena_buckets_used, 1);
    ASSERT(malloc_arena(BUCKET_SIZE), "Should be possible to allocate this");
    EXPECT("buckets in use", arena_buckets_used, 2);
    ASSERT(malloc_arena(1), "Expected alloc to pass");
    EXPECT("buckets in use", arena_buckets_used, 3);
    free_arena();
    ASSERT(arena_buckets_array_size == 0, "Arena not freed?");
    printf("Passed all arena tests\n");
}
