#pragma once
/**
 * Table implementation adapted from Municficient's Crafting Interpreters
 */

#include "common.h"
#include "lexer.h"

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

static void *table_set_token(Table *table, const Token *token, void *entry)
{
    return table_set(table, token->start, token->length, entry);
}
static void *table_get_token(Table *table, const Token *token)
{
    return table_get(table, token->start, token->length);
}


