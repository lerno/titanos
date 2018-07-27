#pragma once
/**
 * Table implementation adapted from Municficient's Crafting Interpreters
 */

#include "common.h"

typedef struct _Entry
{
    const char *key;
    uint32_t key_len;
    uint32_t hash;
    void *value;
} Entry;

typedef struct _Table
{
    uint32_t count;
    uint32_t capacity;
    Entry *entries;
} Table;

void table_init(Table *table, uint32_t initial_size);
void *table_set(Table *table, const char *key, uint32_t len, void *value);
void *table_get(Table *table, const char *key, uint32_t len);
void *table_delete(Table *table, const char *key, uint32_t len);



