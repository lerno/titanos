#pragma once

#include "common.h"

struct _Token;

typedef enum
{
    DIAG_IGNORE = 0,
    DIAG_WARN,
    DIAG_ERROR,
} DiagnosticsSeverity;

typedef enum
{
    DIAG_NONE = 0, // Don't use!
    DIAG_WARNING_TYPE, // Don't use!
    DIAG_UNUSED,
    DIAG_UNUSED_PARAMETER,
    DIAG_UNUSED_FUNCTION,
    DIAG_UNUSED_VARIABLE,
    DIAG_UNUSED_IMPORT,
    DIAG_UNUSED_MODULE,
    DIAG_UNUSED_LABEL,
    DIAG_UNUSED_PUBLIC,
    DIAG_UNUSED_TYPE,
    DIAG_CONVERSION,
    DIAG_COVERED_SWITCH_DEFAULT,
    DIAG_UNREACHABLE_DEFAULT,
    DIAG_ERROR_TYPE, // Don't use this!
    DIAG_FALLOFF_NONVOID,
    DIAG_DUPLICATE_ATTRIBUTE,
    DIAG_NOT_IN_ENUM,
    DIAG_MISSING_CASE,
    DIAG_REMAINDER_DIV_BY_ZERO,
    DIAG_INT_TO_POINTER_CAST,
    DIAG_SHIFT_LHS_NEGATIVE,
    DIAG_SHIFT_NEGATIVE,
    DIAG_SHIFT_GT_TYPEWIDTH,
    DIAG_END
} DiagnosticsType;

typedef struct _Array Array;

void diagnostics_init(void);
void diagnostics_reset(void);
void diagnostics_update_severity(DiagnosticsSeverity severity, DiagnosticsType type);
bool diagnostics_silence_warnings(Array *warnings);
void diagnostics_use_color(bool use_color);
void error_at(struct _Token *token, const char *message);
bool in_panic_mode(void);
void reset_panic_mode(void);
bool error_found(void);



