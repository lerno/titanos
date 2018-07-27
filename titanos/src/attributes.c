#include "attributes.h"


// Please note that this list must match the order of the attribute enum.
static const AttributeInfo attribute_info[] = {
        { ATTRIBUTE_EXPORT, "export", false, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_PACKED, "packed", false, ATTR_TARGET_TYPE },
        { ATTRIBUTE_UNUSED, "unused", false, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_UNUSED_PARAMS, "unused_params", false, ATTR_TARGET_FUNC },
        { ATTRIBUTE_SECTION, "section", true, ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_NORETURN, "noreturn", false, ATTR_TARGET_FUNC },
        { ATTRIBUTE_INLINE, "inline", false, ATTR_TARGET_FUNC },
        { ATTRIBUTE_ALIGNED, "aligned", true, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_WEAK, "weak", false, ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_OPAQUE, "opaque", false, ATTR_TARGET_TYPE },
        { ATTRIBUTE_CNAME, "cname", true, ATTR_TARGET_TYPE | ATTR_TARGET_FUNC | ATTR_TARGET_VAR },
        { ATTRIBUTE_NO_TYPEDEF, "no_typedef", false, ATTR_TARGET_TYPE },
};

AttributeType attribute_type_from_token(Token *token)
{
    for (unsigned i = 0; i < ATTRIBUTE_UNKNOWN; i++)
    {
        if (token_compare_str(token, attribute_info[i].name))
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
