#include <stdint.h>
#include <memory.h>
#include "common.h"
#include <stdlib.h>


typedef struct _Symbol {
	const char *name;
	uint8_t length;
	struct _Type *type;
} Symbol;

#define MAX_SYMBOLS 256
#define MAX_SYMBOL_LENGTH 255

typedef struct _SymbolLink {
	Symbol symbol;
	struct _SymbolLink *next;
} SymbolLink;

typedef struct
{
	uint8_t next_symbol;
	Symbol symbols[MAX_SYMBOLS];
	SymbolLink *symbol_link;
} SymList;

static inline bool symbol_matched_name(Symbol *symbol, const char *name, int length)
{
	return symbol->length == length &&
	       (memcmp(symbol->name, name, (size_t)length) == 0);
}

static inline Symbol *find_symbol_in_symlist(SymList *sym_list, const char *name, int length)
{
	for (int i = sym_list->next_symbol - 1; i >= 0; i--)
	{
		Symbol *symbol = &sym_list->symbols[i];
		if (symbol_matched_name(symbol, name, length)) return symbol;
	}
	SymbolLink *symbol_link = sym_list->symbol_link;
	while (symbol_link != NULL)
	{
		if (symbol_matched_name(&symbol_link->symbol, name, length)) return &symbol_link->symbol;
	}
	return NULL;
}

static inline Symbol *allocate_next_symbol(SymList *sym_list)
{
	if (sym_list->next_symbol < MAX_SYMBOLS - 1)
	{
		return &sym_list->symbols[sym_list->next_symbol++];
	}
	SymbolLink *symbol_link = malloc(sizeof(SymbolLink));
	if (!sym_list->symbol_link)
	{
		sym_list->symbol_link = symbol_link;
		return &symbol_link->symbol;
	}
	SymbolLink *pointer = sym_list->symbol_link;
	while (pointer->next != NULL)
	{
		pointer = pointer->next;
	}
	pointer->next = symbol_link;
	return &symbol_link->symbol;
}

void error_at_current(const char *message);

static inline Symbol *push_symbol_in_symlist(SymList *sym_list, const char *name, int length)
{
	Symbol *symbol = allocate_next_symbol(sym_list);
	symbol->name = name;
	symbol->length = (uint8_t)length;
	if (length > MAX_SYMBOL_LENGTH)
	{
		error_at_current("Symbol exceeded max length");
	}
	return symbol;
}

static Symbol SYMBOL_MISSING;
