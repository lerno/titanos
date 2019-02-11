//
// Created by Christoffer Lernö on 2019-02-01.
//

#include "analyser.h"
#include "scope.h"
#include "error.h"

__thread Analyser *active_analyser = NULL;

void select_analyser(Analyser *current_analyser)
{
    active_analyser = current_analyser;
    active_scope = &current_analyser->scope;
}


#define TABLE_SCOPE_DEPTH 256

__thread static Table tables[TABLE_SCOPE_DEPTH];
__thread int current_table = -1;

Table *push_scratch_table()
{
    LOG_FUNC;
    if (current_table == -1)
    {
        for (unsigned i = 0; i < TABLE_SCOPE_DEPTH; i++)
        {
            table_init(&tables[i], 128);
        }
        current_table = 0;
    }
    if (current_table == TABLE_SCOPE_DEPTH)
    {
        return NULL;
    }
    return &tables[current_table++];
}
void pop_scratch_table(Table *table)
{
    assert(table == &tables[current_table - 1]);
    current_table--;
    table_clear(&tables[current_table]);
}

__thread static Vector scratch_vector;
int current_vector = -1;

Vector *push_scratch_vector()
{
    if (current_vector == -1)
    {
        vector_init(&scratch_vector, 256);
    }
    if (current_vector != 0)
    {
        FATAL_ERROR("Tried to use more than one scratch vector");
    }
    current_vector++;
    return &scratch_vector;
}
void pop_scratch_vector(Vector *vector)
{
    assert(vector == &scratch_vector);
    assert(current_vector == 1);
    current_vector--;
}

