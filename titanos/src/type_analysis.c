//
// Created by Christoffer Lernö on 2019-02-05.
//

#include <string.h>
#include "type_analysis.h"
#include "analyser.h"
#include "error.h"
#include "types/type.h"
#include "diagnostics.h"
#include "expression_analysis.h"
#include "expr.h"

const static uint64_t MAX_ARRAY_SIZE = UINT32_MAX;

bool update_decl(Decl *decl, bool used_public)
{
    assert(decl && "Should only be used on type with decl!");

    decl->is_used = true;

    if (!used_public) return true;

    bool is_external = scope_is_external_module(decl->module);
    if (!is_external && !decl->is_public)
    {
        return false;
    }
    if (is_external) decl->is_used_public = true;

    return true;
}

bool resolve_array_size(TypeOld *type)
{
    assert(type->type_id == XTYPE_ARRAY);
    if (type->array.is_len_resolved) return true;
    assert (!type->array.is_empty && "Empty should always be pre-resolved!");
    Expr *size = type->array.len_expr;
    if (!analyse_expr(size, RHS)) return false;
    if (size->expr_id != EXPR_CONST)
    {
        sema_error_at(size->span, "Array size must be a constant");
        return false;
    }
    if (size->const_expr.value.type != VALUE_TYPE_INT)
    {
        sema_error_at(size->span, "Array size must be an integer");
        return false;
    }
    if (size->const_expr.value.big_int.is_negative)
    {
        sema_error_at(size->span,
                      "Array size must be a positive integer");
        return false;
    }
    uint64_t array_size = size->const_expr.value.big_int.digit_count > 1
                          ? MAX_ARRAY_SIZE + 1
                          : bigint_as_unsigned(&size->const_expr.value.big_int);
    if (array_size > MAX_ARRAY_SIZE)
    {
        sema_error_at(size->span,
                      "Array size cannot exceed %d", MAX_ARRAY_SIZE);
        return false;
    }
    DEBUG_LOG("Array size %d", (int)array_size);
    type->array.is_len_resolved = true;
    type->array.len = (uint32_t)array_size;
    return true;
}

static bool resolve_unresolved_type(TypeOld *type, bool used_public)
{
    assert(type->type_id == XTYPE_UNRESOLVED);
    Decl *decl = NULL;
    Expr *type_expr = type->unresolved.type_expr;
    if (!analyse_expr(type_expr, RHS)) return false;
    const char *module_name = NULL;
    const char *name = NULL;
    switch (type_expr->expr_id)
    {
        case EXPR_ACCESS:
            if (type_expr->access_expr.parent->expr_id != EXPR_IDENTIFIER)
            {
                sema_error_at(type_expr->access_expr.parent->span, "Expected a module name");
                return false;
            }
            module_name = type_expr->access_expr.parent->identifier_expr.identifier;
            if (type_expr->access_expr.sub_element->expr_id != EXPR_IDENTIFIER)
            {
                sema_error_at(type_expr->access_expr.parent->span, "Expected a type name");
                return false;
            }
            name = type_expr->access_expr.sub_element->identifier_expr.identifier;
            break;
        case EXPR_IDENTIFIER:
            name = type_expr->identifier_expr.identifier;
            break;
        case EXPR_TYPE:
            if (resolve_type(type_expr->type_expr.type, used_public))
            {
                type->resolved = type_expr->type_expr.type;
                type->type_id = XTYPE_RESOLVED;
                return true;
            }
            return false;
        default:
            sema_error_at(type_expr->span, "Expected a type");
            return false;
    }
    if (module_name)
    {
        Module *module = scope_find_used_module(module_name, type_expr->span, used_public);
        if (!module) return false;
        decl = scope_find_symbol_in_module(module, name, type_expr->span);
    }
    else
    {
        decl = scope_find_symbol(name, true, used_public, type_expr->span);
    }
    if (!decl)
    {
        // Error message already written
        type->type_id = XTYPE_INVALID;
        return false;
    }
    if (decl->type_id == DECL_ALIAS_TYPE)
    {
        if (!resolve_type(decl->type.type, used_public))
        {
            type->type_id = XTYPE_INVALID;
            return false;
        }
        decl = decl->type.type->decl;
    }
    if (!decl_is_type(decl))
    {
        sema_error_at(type_expr->span, "No type with that name found");
        type->type_id = XTYPE_INVALID;

    }
    type->resolved = decl->type.type;
    type->type_id = XTYPE_RESOLVED;
    if (!update_decl(decl, used_public))
    {
        sema_error_at(type_expr->span, "'%s' is not a public type", decl->name);
        return false;
    }
    return true;
}

bool resolve_type(TypeOld *type, bool used_public)
{
    switch (type->type_id)
    {
        case XTYPE_UNRESOLVED:
            return resolve_unresolved_type(type, used_public);
        case XTYPE_INVALID:
            return false;
        case XTYPE_OPAQUE:
            return resolve_type(type->opaque, used_public);
        case XTYPE_VOID:
        case XTYPE_RESOLVED:
        case XTYPE_CONST_FLOAT:
        case XTYPE_CONST_INT:
        case XTYPE_NIL:
        case XTYPE_INT:
        case XTYPE_BOOL:
        case XTYPE_FLOAT:
        case XTYPE_STRING:
            return true;
        case XTYPE_POINTER:
            return resolve_type(type->pointer.base, used_public);
        case XTYPE_ARRAY:
            return resolve_type(type->array.base, used_public) && resolve_array_size(type);
        case XTYPE_TYPEVAL:
            return resolve_type(type->type_of_type, used_public);
        case XTYPE_ALIAS:
            return resolve_type(type->alias, used_public);
        case XTYPE_ENUM:
        case XTYPE_FUNC:
        case XTYPE_FUNC_TYPE:
        case XTYPE_STRUCT:
        case XTYPE_UNION:
            if (!update_decl(type->decl, used_public))
            {
                FATAL_ERROR("Should not fail!");
            }
            return true;
    }
    UNREACHABLE;
}

static inline bool analyse_enum_type(Decl *decl)
{
    if (!resolve_type(decl->enum_decl.type.type, decl->is_public)) return false;

    TypeOld *type = decl->enum_decl.type.type;

    if (!type_is_int(type))
    {
        sema_error_at(decl->span, "Expected an integer type");
        return false;
    }


    // TODO if this one is public but the alias used isn't, should we flag the enum as an error?

    if (!decl->enum_decl.constants->size)
    {
        sema_error_at(decl->span, "Enum '%s' is empty", decl->name);
        return false;
    }

    bool success = true;

    // TODO what about use of previous constants?
    STable *table = push_scratch_table();
    assert(table && "Should always work since enums aren't nested");

    bool is_first_value = true;
    BigInt current_value;
    bigint_init_signed(&current_value, 0);
    for (unsigned i = 0; i < decl->enum_decl.constants->size; i++)
    {
        Decl *entry = decl->enum_decl.constants->entries[i];
        assert(entry->type_id == DECL_ENUM_CONSTANT);

        entry->module = decl->module;
        if (entry->is_public && entry->module->is_exported) entry->is_exported = true;

        if (!is_upper(entry->name) && !active_analyser->parser->is_interface)
        {
            sema_error_at(entry->span, "Enum values must start with upper case");
            success = false;
        }
        Decl *previous_entry = stable_set(table, entry->name, entry);
        if (previous_entry)
        {
            sema_error_at(entry->span, "Duplicate definition of '%s'", entry->name);
            success = false;
        }

        Expr *value = entry->enum_constant.init_value;
        if (!value)
        {
            value = expr_new(EXPR_CONST, entry->span);
            entry->enum_constant.init_value = value;
            bigint_init_bigint(&value->const_expr.value.big_int, &current_value);
            value->const_expr.value.type = VALUE_TYPE_INT;
            bigint_incr(&current_value);
            is_first_value = false;
            continue;
        }
        if (!analyse_expr(value, RHS))
        {
            success = false;
            is_first_value = false;
            continue;
        }
        if (value->expr_id != EXPR_CONST)
        {
            sema_error_at(entry->span, "Enum value '%s' is not constant", entry->name);
            success = false;
            is_first_value = false;
            continue;
        }
        if (value->const_expr.value.type != VALUE_TYPE_INT)
        {
            sema_error_at(value->span, "Expected an integer constant for '%s'", entry->name);
            success = false;
            is_first_value = false;
            continue;
        }
        BigInt *big_int = &value->const_expr.value.big_int;
        if (big_int->is_negative && !type_is_signed2(type))
        {
            sema_error_at(value->span, "Negative enum value for '%s', must be unsigned", entry->name);
            success = false;
            is_first_value = false;
            continue;
        }
        if (!bigint_fits_in_bits(big_int, type->integer.bits, type_is_signed2(type)))
        {
            sema_error_at(value->span, "Enum value '%s' exceeds the type size", entry->name);
            success = false;
            is_first_value = false;
            continue;
        }

        if (!is_first_value && bigint_cmp(&current_value, &value->const_expr.value.big_int) != INT_LT)
        {
            sema_error_at(value->span, "Enum values must be placed in strictly ascending value order");
            success = false;
        }
        bigint_init_bigint(&current_value, &value->const_expr.value.big_int);
        bigint_incr(&current_value);
        is_first_value = false;
    }
    pop_scratch_table(table);
    return success;
}

static bool analyse_struct_names(Decl *decl, STable *names)
{
    assert(names && decl);
    bool success = true;
    if (decl->struct_decl.is_global && active_analyser->parser->is_interface && decl->name &&
        !is_lower(decl->name))
    {
        sema_error_at(decl->span, "Struct name needs to start with lower case.");
        success = false;
    }

    Vector *members = decl->struct_decl.members;
    for (unsigned  i = 0; i < members->size; i++)
    {
        Decl *member = members->entries[i];
        const char *name = member->name;
        if (!name)
        {
            assert(member->type_id == DECL_STRUCT_TYPE);
            success = analyse_struct_names(member, names) && success;
            continue;
        }
        Decl *old_member = stable_set(names, name, member);
        if (old_member)
        {
            sema_error_at(member->span, "Duplicate member '%s'", name);
            prev_at(old_member->span, "Previous definition was here");
            success = false;
            continue;
        }
        if (member->type_id == DECL_STRUCT_TYPE)
        {
            STable *sub_names = push_scratch_table();
            if (sub_names == NULL)
            {
                sema_error_at(member->span, "Struct nesting is too deep");
                success = false;
                continue;
            }
            success = analyse_struct_names(member, sub_names) && success;
            pop_scratch_table(sub_names);
        }
    }
    if (!success) return false;

    return success;
}

bool analyse_func_decl(Decl *decl, bool is_public)
{
    assert(decl->type_id == DECL_FUNC);
    bool success = resolve_type(decl->func_decl.rtype.type, is_public);
    if (decl->func_decl.rtype.type->type_id == XTYPE_OPAQUE && is_public)
    {
        sema_error_at(decl->span, "Opaque type returned for public function");
        success = false;
    }
    for (unsigned i = 0; i < decl->func_decl.args->size; i++)
    {
        Decl *param_decl = decl->func_decl.args->entries[i];
        assert(param_decl->type_id == DECL_VAR && param_decl->var.kind == VARDECL_PARAM);
        success = resolve_type(param_decl->type.type, is_public) && success;
        if (param_decl->type.type->type_id == XTYPE_INVALID) continue;
    }
    return success;
}

static inline bool analyse_func_type(Decl *decl)
{
    assert(decl->type_id == DECL_FUNC_TYPE);
    assert(decl->is_public == decl->func_type.func_decl->is_public);
    return analyse_func_decl(decl->func_type.func_decl, decl->is_public);
}

static inline bool analyse_struct_type(Decl *decl)
{
    STable *table = push_scratch_table();
    assert(table && "Should always work");
    bool success = analyse_struct_names(decl, table);
    pop_scratch_table(table);
    return success;
}
static inline bool analyse_type_decl(Decl *decl)
{
    LOG_FUNC;

    // check extra stuff depending on subclass
    switch (decl->type_id)
    {
        case DECL_FUNC:
        case DECL_VAR:
        case DECL_ARRAY_VALUE:
        case DECL_ENUM_CONSTANT:
        case DECL_IMPORT:
        case DECL_LABEL:
            FATAL_ERROR("Does not have type");
        case DECL_BUILTIN:
            // Always resolved.
            return true;
        case DECL_ALIAS_TYPE:
            return resolve_type(decl->type.type, decl->is_public);
        case DECL_STRUCT_TYPE:
            return analyse_struct_type(decl);
        case DECL_ENUM_TYPE:
            return analyse_enum_type(decl);
        case DECL_FUNC_TYPE:
            return analyse_func_type(decl);
    }
    return false;
}

bool analyse_attributes(Decl* decl)
{
    LOG_FUNC;

    Vector *attributes = decl->attributes;

    if (decl->type_id == DECL_FUNC_TYPE)
    {
        // For FuncType use attributes of the underlying function, since there's where it's stored.
        assert(!attributes);
        attributes = decl->func_type.func_decl->attributes;
    }

    // No attributes?
    if (!attributes) return true;

    // Clear our scratch table
    STable *table = push_scratch_table();
    assert(table && "Should never happen since we don't have nested use");

    bool success = true;
    for (unsigned a = 0; a < attributes->size; a++)
    {
        Ast *attribute = attributes->entries[a];
        assert(attribute->ast_id == AST_ATTRIBUTE);
        const char *name = attribute->attribute.name;
        Ast *old = stable_set(table, attribute->attribute.name, attribute);
        if (old)
        {
            sema_error_at(attribute->span, "Attribute %s appears more than once in attribute list", name);
            success = false;
            continue;
        }
        AttributeType attribute_type = attribute_type_from_string(name);
        switch (attribute_type)
        {
            case ATTRIBUTE_UNKNOWN:
                break;
            case ATTRIBUTE_EXPORT:
                if (!decl->is_public)
                {
                    sema_error_at(attribute->span, "Cannot use export on non-public declaration");
                    success = false;
                }
                else
                {
                    decl->is_exported = true;
                }
                break;
            case ATTRIBUTE_PACKED:
                if (decl->type_id != DECL_STRUCT_TYPE)
                {
                    sema_error_at(attribute->span, "@packed can only be used with structs");
                    success = false;
                }
            case ATTRIBUTE_UNUSED:
            case ATTRIBUTE_UNUSED_PARAMS:
                break;
            case ATTRIBUTE_SECTION:
                if (!analyse_expr(attribute->attribute.value, RHS))
                {
                    success = false;
                    break;
                }
                if (attribute->attribute.value->expr_id != EXPR_CONST
                    || attribute->attribute.value->const_expr.value.type != VALUE_TYPE_STRING)
                {
                    sema_error_at(attribute->attribute.value->span, "Expected a string argument for @section");
                    success = false;
                    break;
                }
                if (attribute->attribute.value->const_expr.value.str_len == 0)
                {
                    sema_error_at(attribute->span, "A non empty string is required for @section");
                    success = false;
                    break;
                }
                break;
            case ATTRIBUTE_INLINE:
            case ATTRIBUTE_NORETURN:
                // TODO errors
                break;
            case ATTRIBUTE_ALIGNED:
                if (!analyse_expr(attribute->attribute.value, RHS))
                {
                    success = false;
                    break;
                }
                if (attribute->attribute.value->expr_id != EXPR_CONST
                    || attribute->attribute.value->const_expr.value.type != VALUE_TYPE_INT)
                {
                    sema_error_at(attribute->attribute.value->span, "Expected a integer argument for @aligned");
                    success = false;
                    break;
                }
                if (attribute->attribute.value->const_expr.value.big_int.is_negative)
                {
                    sema_error_at(attribute->attribute.value->span, "Expected a positive integer for @aligned");
                    success = false;
                    break;
                }
                if (bigint_popcount_unsigned(&attribute->attribute.value->const_expr.value.big_int) != 1)
                {
                    sema_error_at(attribute->attribute.value->span, "@aligned must be a power of 2");
                    success = false;
                    // TODO check if alignment is too small (smaller then size of type)
                    break;
                }
                break;
            case ATTRIBUTE_WEAK:
                if (!decl->is_public || !decl->is_exported)
                {
                    // TODO ordering?!
                    sema_error_at(attribute->span, "@weak can only be used for public, exported declarations");
                    success = false;
                    break;
                }
                break;
            case ATTRIBUTE_OPAQUE:
                if (decl->type_id != DECL_STRUCT_TYPE)
                {
                    sema_error_at(attribute->span, "@opaque can only be used with structs");
                    success = false;
                    break;
                }
                if (!decl->is_public)
                {
                    sema_error_at(attribute->span, "@opaque can only be used public structs");
                    success = false;
                    break;
                }
                break;
            case ATTRIBUTE_CNAME:
                if (!active_analyser->parser->is_interface)
                {
                    sema_error_at(attribute->span, "@cname can only be used with interfaces");
                    success = false;
                    break;
                }
                decl->has_cname = true;
                break;
            case ATTRIBUTE_NO_TYPEDEF:
                if (decl->type_id != DECL_STRUCT_TYPE)
                {
                    sema_error_at(attribute->span, "@no_typedef can only be used with structs");
                    success = false;
                    break;
                }
                if (!active_analyser->parser->is_interface)
                {
                    sema_error_at(attribute->span, "@no_typedef can only be used with interfaces");
                    success = false;
                    break;
                }
                decl->struct_decl.no_typedef = true;
                break;
        }
    }
    pop_scratch_table(table);
    return success;
}

void analyse_types(void)
{
    bool success = true;
    for (unsigned i = 0; i < active_analyser->parser->types->size; i++)
    {
        Decl *decl = active_analyser->parser->types->entries[i];
        success = (analyse_type_decl(decl) && analyse_attributes(decl)) && success;
        decl_print(decl, 0);
    }
}
