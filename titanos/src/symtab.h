#pragma once
//
// Created by Christoffer Lernö on 2019-02-22.
//


#include "common.h"
#include "lexer.h"


void symtab_init(uint32_t max_size);
const char *symtab_add(const char *key, uint32_t len);

typedef struct _VoidEntry
{
    const char *key;
    void *value;
} SEntry;

typedef struct _STable
{
    uint32_t count;
    uint32_t capacity;
    SEntry *entries;
} STable;


void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);
void *stable_delete(STable *table, const char *key);
void stable_clear(STable *table);
