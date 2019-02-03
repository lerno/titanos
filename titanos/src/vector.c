#include <assert.h>
#include <string.h>
#include "vector.h"
#include "arena_allocator.h"
#include "error.h"

#define MAX_VECTOR_SIZE 1024

void vector_init(Vector *vector, unsigned size)
{
    vector->entries = size ? malloc_arena(size * sizeof(void *)) : NULL;
    vector->size = 0;
    vector->reserved = size;
    vector->entries = malloc_arena(size * sizeof(void *));
}


Vector *new_vector(unsigned size)
{
    Vector *vector = malloc_arena(sizeof(Vector));
    vector_init(vector, size);
    return vector;
}

void vector_add(Vector *vector, void *element)
{
    if (vector->reserved == vector->size)
    {
        ASSERT(vector->reserved < MAX_VECTOR_SIZE, "Vector size too large, exceeded %d", MAX_VECTOR_SIZE);

        unsigned old = vector->reserved;
        vector->reserved = old ? old * 2 : 8;
        void **new_data = malloc_arena(vector->reserved * sizeof(void *));
        memcpy(new_data, vector->entries, old * sizeof(void *));
        vector->entries = new_data;
    }

    vector->entries[vector->size++] = element;
}

void vector_copy(Vector *dst, Vector *src)
{
    dst->size = 0;
    for (unsigned i = 0; i < src->size; i++)
    {
        vector_add(dst, src[i].entries[i]);
    }
}

void *vector_remove(Vector *vector)
{
    if (!vector->size) return NULL;
    return vector->entries[--vector->size];
}

void *vector_last(Vector *vector)
{
    return vector->size ? vector->entries[vector->size - 1] : NULL;
}

int vector_find(Vector *vector, void *element)
{
    for (unsigned i = 0; i < vector->size; i++)
    {
        if (vector->entries[i] == element) return i;
    }
    return -1;
}


void run_vector_tests(void)
{
    init_arena();
    Vector *vector = new_vector(8);
    EXPECT("size", vector->size, 0);
    ASSERT(NULL == vector_last(vector), "Last entry not null?");
    ASSERT(NULL == vector_remove(vector), "Can't remove last entry when null?");
    vector_add(vector, NULL);
    EXPECT("size", vector->size, 1);
    vector_add(vector, malloc_arena(2));
    EXPECT("size", vector->size, 2);
    EXPECT("reserved", vector->reserved, 8);
    vector_add(vector, malloc_arena(2));
    vector_add(vector, malloc_arena(2));
    vector_add(vector, malloc_arena(2));
    vector_add(vector, malloc_arena(2));
    vector_add(vector, malloc_arena(2));
    EXPECT("size", vector->size, 7);
    EXPECT("reserved", vector->reserved, 8);
    vector_add(vector, malloc_arena(2));
    EXPECT("size", vector->size, 8);
    EXPECT("reserved", vector->reserved, 8);
    void *ptr_8 = vector->entries[7];
    void *ptr_last = malloc_arena(2);
    vector_add(vector, ptr_last);
    EXPECT("size", vector->size, 9);
    EXPECT("reserved", vector->reserved, 16);
    EXPECT("find", vector_find(vector, ptr_last), 8);
    ASSERT(ptr_8 == vector->entries[7], "Memcopy appeared to fail :(");
    ASSERT(ptr_last == vector_last(vector), "Unexpected last element");
    ASSERT(ptr_last == vector_remove(vector), "Unexpected removed element");
    ASSERT(vector_find(vector, ptr_last) == -1, "Found removed element?!");
    EXPECT("size", vector->size, 8);
    EXPECT("reserved", vector->reserved, 16);
    ASSERT(ptr_last != vector_last(vector), "Unexpected last element");
    free_arena();
    printf("Passed all vector tests.\n");
}
