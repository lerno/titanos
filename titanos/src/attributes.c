#include <string.h>
#include "attributes.h"


// Please note that this list must match the order of the attribute enum.
static const AttributeInfo attribute_info[] = {
        { ATTRIBUTE_EXPORT, "export", ATTR_ARG_NONE, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_PACKED, "packed", ATTR_ARG_NONE, ATTR_TARGET_TYPE },
        { ATTRIBUTE_UNUSED, "unused", ATTR_ARG_NONE, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_UNUSED_PARAMS, "unused_params", ATTR_ARG_NONE, ATTR_TARGET_FUNC },
        { ATTRIBUTE_SECTION, "section", ATTR_ARG_STRING, ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_NORETURN, "noreturn", ATTR_ARG_NONE, ATTR_TARGET_FUNC },
        { ATTRIBUTE_INLINE, "inline", ATTR_ARG_NONE, ATTR_TARGET_FUNC },
        { ATTRIBUTE_ALIGNED, "aligned", ATTR_ARG_UINT, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_WEAK, "weak", ATTR_ARG_NONE, ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_OPAQUE, "opaque", ATTR_ARG_NONE, ATTR_TARGET_TYPE },
        { ATTRIBUTE_CNAME, "cname", ATTR_ARG_STRING, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_NO_TYPEDEF, "no_typedef", ATTR_ARG_NONE, ATTR_TARGET_TYPE },
};


AttributeType attribute_type_from_string(const char *name)
{
    for (unsigned i = 0; i < ATTRIBUTE_UNKNOWN; i++)
    {
        if (strcmp(name, attribute_info[i].name) == 0)
        {
            return attribute_info[i].type;
        }
    }
    return ATTRIBUTE_UNKNOWN;
}




const AttributeInfo *attribute_info_from_type(AttributeType type)
{
    return &attribute_info[type];
}
