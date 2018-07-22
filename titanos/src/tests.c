#include "tests.h"
#include "arena_allocator.h"
#include "vector.h"

void test_all(void)
{
    run_arena_allocator_tests();
    run_vector_tests();
}
