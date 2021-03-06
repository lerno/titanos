#pragma once

#include "lexer.h"


typedef enum _AttributeTarget
{
    ATTR_TARGET_FUNC = 0x01,
    ATTR_TARGET_TYPE = 0x02,
    ATTR_TARGET_VAR = 0x04,
} AttributeTarget;

typedef enum _AttributeArgument
{
    ATTR_ARG_NONE,
    ATTR_ARG_STRING,
    ATTR_ARG_BOOL,
    ATTR_ARG_UINT,
    ATTR_ARG_INT,
    ATTR_ARG_NUMBER,
} AttributeArgument;

// Do not reorder this list
typedef enum _AttributeType
{
    ATTRIBUTE_EXPORT,
    ATTRIBUTE_PACKED,
    ATTRIBUTE_UNUSED,
    ATTRIBUTE_UNUSED_PARAMS,
    ATTRIBUTE_SECTION,
    ATTRIBUTE_NORETURN,
    ATTRIBUTE_INLINE,
    ATTRIBUTE_ALIGNED,
    ATTRIBUTE_WEAK,
    ATTRIBUTE_OPAQUE,
    ATTRIBUTE_CNAME,
    ATTRIBUTE_NO_TYPEDEF,
    ATTRIBUTE_UNKNOWN, // This must always be the last attribute type
} AttributeType;

typedef struct _AttributeInfo
{
    AttributeType type;
    const char *name;
    AttributeArgument argument;
    unsigned targets;
} AttributeInfo;

AttributeType attribute_type_from_token(Token *token);
const AttributeInfo *attribute_info_from_type(AttributeType type);
