#pragma once

typedef struct _Vector
{
    void **entries;
    unsigned size;
    unsigned reserved;
} Vector;

Vector *new_vector(unsigned start_size);
void vector_add(Vector *vector, void *element);
void *vector_remove(Vector *vector);
void *vector_last(Vector *vector);
int vector_find(Vector *vector, void *element);
void vector_init(Vector *vector, unsigned size);

void run_vector_tests(void);

