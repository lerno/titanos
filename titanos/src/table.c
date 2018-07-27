#include <string.h>
#include "table.h"
#include "common.h"
#include "arena_allocator.h"
#include "error.h"

#define TABLE_MAX_LOAD 0.75
#define MAX_HASH_SIZE (1024 * 1024)
static uint32_t hash_string(const char *key, uint32_t len)
{
    uint32_t hash = 2166136261u;
    for (int i = 0; i < len; i++)
    {
        hash ^= key[i];
        hash *= 16777619;
    }
    return hash;
}

void table_init(Table *table, uint32_t initial_size)
{
    assert(initial_size && "Size must be larger than 0");
    Entry *entries = malloc_arena(initial_size * sizeof(Entry));
    for (uint32_t i = 0; i < initial_size; i++)
    {
        entries[i].key = NULL;
        entries[i].value = NULL;
    }
    table->count = 0;
    table->capacity = initial_size;
    table->entries = entries;
}

#define TOMBSTONE ((void *)0x01)
static Entry *entry_find(Entry *entries, uint32_t capacity, const char *key, uint32_t key_len, uint32_t hash)
{
    uint32_t index = hash % capacity;
    Entry *tombstone = NULL;
    while (true)
    {
        Entry *entry = &entries[index];
        if (entry->key == key && entry->key_len == key_len) return entry;
        if (entry->key == NULL)
        {
            if (entry->value != TOMBSTONE)
            {
                return tombstone ? tombstone : entry;
            }
            else
            {
                if (!tombstone) tombstone = entry;
            }
        }
        index = (index + 1) % capacity;
    }
}


void *table_set(Table *table, const char *key, uint32_t len, void *value)
{
    assert(value && "Cannot insert NULL");
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD)
    {
        ASSERT(table->capacity < MAX_HASH_SIZE, "Table size too large, exceeded %d", MAX_HASH_SIZE);

        uint32_t new_capacity = table->capacity ? table->capacity * 2 : 16;
        Entry *new_data = malloc_arena(new_capacity * sizeof(Entry));
        for (uint32_t i = 0; i < new_capacity; i++)
        {
            new_data[i].key = NULL;
            new_data[i].value = NULL;
        }
        table->count = 0;
        for (uint32_t i = 0; i < table->capacity; i++)
        {
            Entry *entry = &table->entries[i];
            if (!entry->key) continue;
            table->count++;
            Entry *dest = entry_find(new_data, new_capacity, entry->key, entry->key_len, entry->hash);
            *dest = *entry;
        }
        table->entries = new_data;
        table->capacity = new_capacity;
    }

    uint32_t hash = hash_string(key, len);
    Entry *entry = entry_find(table->entries, table->capacity, key, len, hash);
    void *old = entry->value && entry->value != TOMBSTONE ? entry->value : NULL;
    entry->key = key;
    entry->key_len = len;
    entry->value = value;
    entry->hash = hash;
    if (!old) table->count++;
    return old;
}

void *table_get(Table *table, const char *key, uint32_t len)
{
    if (!table->entries) return NULL;
    uint32_t hash = hash_string(key, len);
    Entry *entry = entry_find(table->entries, table->capacity, key, len, hash);
    return entry->key == NULL ? NULL : entry->value;
}

void *table_delete(Table *table, const char *key, uint32_t len)
{
    if (!table->count) return NULL;
    uint32_t hash = hash_string(key, len);
    Entry *entry = entry_find(table->entries, table->capacity, key, len, hash);
    if (!entry->key) return NULL;
    void *value = entry->value;
    entry->key = NULL;
    entry->value = TOMBSTONE;
    return value;
}
