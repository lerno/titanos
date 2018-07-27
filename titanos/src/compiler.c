#include "compiler.h"
#include "lexer.h"
#include "arena_allocator.h"
#include "diagnostics.h"
#include "semantic_analysis.h"
#include "parsing.h"

bool compile(const char *source)
{
   	init_arena();
   	init_lexer(source);
	diagnostics_init();
    if (!parse()) goto end;
	if (!analyse()) goto end;
    //analyse(NULL);
    end:
    free_arena();
   	return !error_found();
}
