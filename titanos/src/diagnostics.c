
#include <stdio.h>
#include <string.h>
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

void error_at(struct _Token *token, const char *message)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;

	bool show_line = false;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF)
	{
		fprintf(stderr, " at end");
	}
	else if (token->type == TOKEN_ERROR)
	{
		// Nothing.
	}
	else
	{
		fprintf(stderr, " at '%.*s'", token->length, token->start);
		show_line = true;
	}

	fprintf(stderr, ": %s\n", message);

	if (show_line) {
		const char* error_line_start = line_start(token);
		const char* error_line_end = line_end(token);
		fprintf(stderr, "%.*s\n", (int)(error_line_end - error_line_start), error_line_start);
		for (unsigned i = 0; i < (token->start - error_line_start); i++) {
			fprintf(stderr, " ");
		}
		for (int i = 0; i < token->length; i++)
		{
			fprintf(stderr, "^");
		}
		fprintf(stderr, "\n");
	}
	diagnostics.had_error = true;
}

bool error_found()
{
    return diagnostics.had_error;
}


