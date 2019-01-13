
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "diagnostics.h"
#include "lexer.h"
#include "array.h"
#include "error.h"

typedef struct _Diagnostics
{
    bool panic_mode;
    bool had_error;
    bool use_color;
    DiagnosticsSeverity severity[DIAG_END];
} Diagnostics;

Diagnostics diagnostics;

void diagnostics_init(void)
{
	diagnostics.use_color = false;
    diagnostics.panic_mode = false;
    diagnostics.had_error = false;
    for (int i = DIAG_NONE; i < DIAG_WARNING_TYPE; i++)
	{
		diagnostics.severity[i] = DIAG_IGNORE;
	}
	for (int i = DIAG_WARNING_TYPE; i < DIAG_ERROR_TYPE; i++)
	{
		diagnostics.severity[i] = DIAG_WARN;
	}
	for (int i = DIAG_ERROR_TYPE; i < DIAG_END; i++)
	{
		diagnostics.severity[i] = DIAG_ERROR;
	}

}

void diagnostics_reset(void)
{
	diagnostics.panic_mode = false;
	diagnostics.had_error = false;

}
bool in_panic_mode(void)
{
    return diagnostics.panic_mode;
}

void reset_panic_mode(void)
{
    diagnostics.panic_mode = false;
}

void diagnostics_use_color(bool use_color)
{
	diagnostics.use_color = use_color;
}

void diagnostics_update_severity(DiagnosticsSeverity severity, DiagnosticsType type)
{
    diagnostics.severity[type] = severity;
}

bool diagnostics_silence_warnings(Array *warnings)
{
	for (unsigned i = 0; i < warnings->count; i++)
	{
		const char *warning = warnings->entries[i];
		if (strcmp("no-unused", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED);
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_PARAMETER);
			continue;
		}
		if (strcmp("no-unused-variable", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_VARIABLE);
			continue;
		}
		if (strcmp("no-unused-function", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_FUNCTION);
			continue;
		}
		if (strcmp("no-unused-type", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_TYPE);
			continue;
		}
		if (strcmp("no-unused-module", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_MODULE);
			continue;
		}
		if (strcmp("no-unused-public", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_PUBLIC);
			continue;
		}
		if (strcmp("no-unused-import", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_IMPORT);
			continue;
		}
		if (strcmp("no-unused-label", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_LABEL);
			continue;
		}
		PRINT_ERROR("recipe has unknown warning: '%s'\n", warning);
		return false;
	}
	return true;
}

static void print_error(struct _Token *token, const char *message)
{
   	File *file = token_get_file(token);
   	const static int backtrace_len = 3;

   	const char *backtrace[backtrace_len + 1];

   	int max_line_length = (int)round(log10(token->line)) + 1;

   	char number_buffer[20];
   	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);

   	if (token->type != TOKEN_EOF)
   	{
   		backtrace[0] = line_start(token);

   		for (int i = 1; i < backtrace_len + 1; i++)
   		{
   			backtrace[i] = find_line_start(file->contents, skip_to_end_of_previous_line(file->contents, backtrace[i - 1] - 1));
   		}
   		for (int i = backtrace_len; i >= 1; i--)
   		{
   			if (backtrace[i] == backtrace[i - 1]) continue;
   			fprintf(stderr, number_buffer, token->line - i, (int)(find_line_end(backtrace[i]) - backtrace[i]), backtrace[i]);
   		}
   		fprintf(stderr, number_buffer, token->line, (int)(find_line_end(token->start) - backtrace[0]), backtrace[0]);
   		for (unsigned i = 0; i < max_line_length + 2 + (token->start - backtrace[0]); i++)
   		{
   			fprintf(stderr, " ");
   		}
   		for (int i = 0; i < token->length; i++)
   		{
   			fprintf(stderr, "^");
   		}
   		fprintf(stderr, "\n");
   		fprintf(stderr, "(%s:%d) Error: %s\n", file->name, token->line, message);
   	}
   	else
   	{
   		fprintf(stderr, "(%s:%d) Error at end of file: %s.\n", file->name, token->line, message);
   	}
   	diagnostics.had_error = true;

}

void vprint_error(struct _Token *token, const char *message, va_list args)
{
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
    print_error(token, buffer);
}

void error_at(struct _Token *token, const char *message, ...)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	va_list args;
	va_start(args, message);
	vprint_error(token, message, args);
	va_end(args);
}

void verror_at(struct _Token *token, const char *message, va_list args)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	vprint_error(token, message, args);
}


void sema_error_at(struct _Token *token, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	vprint_error(token, message, args);
	va_end(args);
}

bool error_found()
{
    return diagnostics.had_error;
}


