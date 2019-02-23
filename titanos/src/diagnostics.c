
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
    unsigned errors;
    unsigned warnings;
    bool use_color;
    DiagnosticsSeverity severity[DIAG_END];
} Diagnostics;

Diagnostics diagnostics;

void diagnostics_init(void)
{
	diagnostics.use_color = false;
    diagnostics.panic_mode = false;
    diagnostics.errors = 0;
    diagnostics.warnings = 0;
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
	diagnostics.errors = 0;

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

typedef enum
{
    PRINT_TYPE_ERROR,
    PRINT_TYPE_PREV,
    PRINT_TYPE_WARN
} PrintType;

static void print_error(SourceRange source_range, const char *message, PrintType print_type)
{
   	File *file = source_get_file(source_range.loc);
   	const static int backtrace_len = 3;

   	Line *line = file_source_line(file, source_range.loc);
   	int max_line_length = (int)round(log10(line->number)) + 1;

   	char number_buffer[20];
   	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);

    for (int i = 1; i < backtrace_len; i++)
    {
    	int line_number = line->number - i;
    	if (line_number < 1) continue;
    	Line *backtrace_line = file->line_start->entries[line_number - 1];
        fprintf(stderr, number_buffer, backtrace_line->number, backtrace_line->length, backtrace_line->start + file->contents);
    }
	fprintf(stderr, number_buffer, line->number, line->length, line->start + file->contents);
    for (unsigned i = 0; i < max_line_length + 2 + line->start - source_range.length; i++)
    {
        fprintf(stderr, " ");
    }
    for (int i = 0; i < source_range.length; i++)
    {
        fprintf(stderr, "^");
    }
    fprintf(stderr, "\n");

    switch (print_type)
    {
        case PRINT_TYPE_ERROR:
            fprintf(stderr, "(%s:%d) Error: %s\n", file->name, line->number, message);
            break;
        case PRINT_TYPE_PREV:
            fprintf(stderr, "(%s:%d) %s\n", file->name, line->number, message);
            break;
        case PRINT_TYPE_WARN:
            fprintf(stderr, "(%s:%d) Warning: %s\n", file->name, line->number, message);
            break;
    }
}

static void vprint_error(SourceRange span, const char *message, va_list args)
{
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
    print_error(span, buffer, PRINT_TYPE_ERROR);
}

void error_at(SourceRange span, const char *message, ...)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	va_list args;
	va_start(args, message);
	vprint_error(span, message, args);
	va_end(args);
	diagnostics.errors++;
}

void verror_at(SourceRange span, const char *message, va_list args)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	vprint_error(span, message, args);
	diagnostics.errors++;
}


void prev_at(SourceRange span, const char *message, ...)
{
    va_list args;
    va_start(args, message);
    char buffer[256];
    vsnprintf(buffer, 256, message, args);
    print_error(span, buffer, PRINT_TYPE_PREV);
    va_end(args);
}

void sema_error_at(SourceRange token, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	vprint_error(token, message, args);
	va_end(args);
	diagnostics.errors++;
}

void sema_warn_at(DiagnosticsType type,SourceRange span, const char *message, ...)
{
    switch (diagnostics.severity[type])
    {
        case DIAG_IGNORE:
            return;
        case DIAG_WARN:
            break;
        case DIAG_ERROR:
        {
            va_list args;
            va_start(args, message);
            vprint_error(span, message, args);
            va_end(args);
            diagnostics.errors++;
            return;
        }
    }
    va_list args;
	va_start(args, message);
    char buffer[256];
    vsnprintf(buffer, 256, message, args);
    if (diagnostics.severity[type])
	print_error(span, buffer, PRINT_TYPE_WARN);
	va_end(args);
	diagnostics.warnings++;
}

unsigned errors()
{
	return diagnostics.errors;
}

bool error_found()
{
    return diagnostics.errors > 0;
}


