//
// Created by Christoffer Lernö on 2019-02-05.
//

#include "type_analysis.h"
#include "analyser.h"
#include "error.h"
#include "types/type.h"
#include "diagnostics.h"
#include "constant_folding.h"

const static uint64_t MAX_ARRAY_SIZE = UINT32_MAX;

static Type *resolve_type_expr(Ast *type_expr, bool used_public)
{
    if (type_expr->type_expr.flags.resolved)
    {
        return NULL;
    }
    bool resolves = false;
    switch (type_expr->type_expr.type)
    {
        case TYPE_EXPR_IDENTIFIER:
        {
            // TODO module symbol
            Token *name = &type_expr->type_expr.identifier_type_expr.name;
            Decl *symbol = scope_find_symbol(&analyser->scope, name, true, used_public);
            if (!symbol)
            {
                sema_error_at(&type_expr->span, "Unknown type '%.*s'",
                              SPLAT_TOK(type_expr->type_expr.identifier_type_expr.name));
                return NULL;
            }
            if (!decl_is_type(symbol))
            {
                sema_error_at(&type_expr->span, "'%.*s' is not a type ",
                              SPLAT_TOK(type_expr->type_expr.identifier_type_expr.name));
                return NULL;
            }
            // TODO
            return NULL;
        }
        case TYPE_EXPR_ARRAY:
        {
            Type *base_type = resolve_type_expr(type_expr->type_expr.array_type_expr.type, used_public);
            Ast *size = type_expr->type_expr.array_type_expr.size;
            if (CONST_FULL != evaluate_constant(type_expr->type_expr.array_type_expr.size))
            {
                sema_error_at(&type_expr->type_expr.array_type_expr.size->span, "Array size must be a constant");
                return NULL;
            }
            if (size->const_expr.value.type != VALUE_TYPE_INT)
            {
                sema_error_at(&type_expr->type_expr.array_type_expr.size->span, "Array size must be an integer");
                return NULL;
            }
            if (size->const_expr.value.big_int.is_negative)
            {
                sema_error_at(&type_expr->type_expr.array_type_expr.size->span,
                              "Array size must be a positive integer");
                return NULL;
            }
            uint64_t array_size = size->const_expr.value.big_int.digit_count > 1
                                  ? MAX_ARRAY_SIZE + 1
                                  : bigint_as_unsigned(&size->const_expr.value.big_int);
            if (array_size > MAX_ARRAY_SIZE)
            {
                sema_error_at(&type_expr->type_expr.array_type_expr.size->span,
                              "Array size cannot exceed %d", MAX_ARRAY_SIZE);
                return NULL;
            }
            // TODO wrong
            Type *type = new_type(TYPE_ARRAY, false, NULL);
            type->array.base = base_type;
            type->array.len = (uint32_t) array_size;
            return type;
        }
        case TYPE_EXPR_POINTER:
        {
            Type *base_type = resolve_type_expr(type_expr->type_expr.array_type_expr.type, used_public);
            if (!base_type) return NULL;
            Type *type = new_type(TYPE_POINTER, false, NULL);
            type->pointer.base = base_type;
            return type;
        }
        case TYPE_EXPR_VOID:
            // TODO
            return new_type(TYPE_VOID, false, NULL);
        default:
            FATAL_ERROR("Unsupported type expr");
    }
}

bool analyse_unresolved_type(Type *type, bool used_public)
{
    assert(type->type_id == TYPE_UNRESOLVED);
    //IdentifierExpr* moduleName = type->getModuleName();
    //IdentifierExpr* typeName = type->getTypeName();
    Decl* decl = NULL;
    if (type->unresolved.module_name.length)
    {
        Module *module = scope_find_used_module(&analyser->scope, &type->module_name, used_public);
        if (!module) return false;
        decl = scope_find_symbol_in_module(&analyser->scope, &type->unresolved.identifier, module);
    }
    else
    {
        decl = scope_find_symbol(&analyser->scope, &type->unresolved.identifier, true, used_public);
    }
    if (!decl) return false;
    if (!decl_is_type(decl))
    {
        sema_error_at(&type->span, "No type with that name found");
        return false;
    }
    bool is_external = scope_is_external_module(&analyser->scope, decl->module);
    if (used_public && !is_external && !decl->is_public)
    {
        sema_error_at(&type->span, "'%.*s' is not a public type", SPLAT_TOK(decl->name));
        return false;
    }
    decl->is_used = true;
    if (used_public || is_external) decl->is_used_public = true;
    // TODO actually resolve!
    return false;
}

static bool resolve_type(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_UNRESOLVED:
            // THIS is incorrect, resolve_type_expr should evaluate and update the type.
            resolve_type_expr(type->unresolved.type_expr, type->is_public);
            return false;
            break;
        case TYPE_INVALID:
            return false;
        default:
            return true;
    }
}

static inline bool check_enum_type(Decl *decl_enum)
{
    if (!resolve_type(decl_enum->type)) return false;

    if (decl_enum->type->type_id != TYPE_INT)
    {
        sema_error_at(&decl_enum->span, "Expected an integer type");
        return false;
    }

    // TODO if this one is public but the alias used isn't, should we flag the enum as an error?

    if (!decl_enum->enumeration.constants->size)
    {
        sema_error_at(&decl_enum->name, "Enum '%.*s' is empty", SPLAT_TOK(decl_enum->name));
        return false;
    }

    bool success = true;

    // TODO what about use of previous constants?
    Table table;
    table_init(&table, decl_enum->enumeration.constants->size * 2 + 2);

    bool is_first_value = true;
    BigInt current_value;
    bigint_init_signed(&current_value, 0);
    for (unsigned i = 0; i < decl_enum->enumeration.constants->size; i++)
    {
        Decl *entry = decl_enum->enumeration.constants->entries[i];
        assert(entry->type_id == DECL_ENUM_CONSTANT);

        entry->module = decl_enum->module;
        if (entry->is_public && entry->module->is_exported) entry->is_exported = true;

        if (!is_upper(&entry->name) && !analyser->parser->is_interface)
        {
            sema_error_at(&entry->name, "Enum values must start with upper case");
            success = false;
        }
        Ast *previous_entry = table_set_token(&table, &entry->name, entry);
        if (previous_entry)
        {
            sema_error_at(&entry->span, "Duplicate definition of '%.*s'", SPLAT_TOK(entry->name));
            success = false;
        }

        Ast *value = entry->enum_constant.init_value;
        if (!value)
        {
            value = new_ast_with_span(AST_CONST_EXPR, &entry->span);
            entry->enum_constant.init_value = value;
            bigint_init_bigint(&value->const_expr.value.big_int, &current_value);
            value->const_expr.value.type = VALUE_TYPE_INT;
            value->const_state = CONST_FULL;
            bigint_incr(&current_value);
            is_first_value = false;
            continue;
        }
        if (!evaluate_constant(value))
        {
            sema_error_at(&entry->span, "Enum value '%.*s' is not constant", SPLAT_TOK(entry->name));
            success = false;
            is_first_value = false;
            continue;
        }
        if (value->const_expr.value.type != VALUE_TYPE_INT)
        {
            sema_error_at(&value->span, "Expected an integer constant for '%.*s'", SPLAT_TOK(entry->name));
            success = false;
            is_first_value = false;
            continue;
        }
        BigInt *big_int = &value->const_expr.value.big_int;
        if (big_int->is_negative && !decl_enum->type->integer.is_signed)
        {
            sema_error_at(&value->span, "Negative enum value for '%.*s', must be unsigned", SPLAT_TOK(entry->name));
            success = false;
            is_first_value = false;
            continue;
        }
        if (!bigint_fits_in_bits(big_int, decl_enum->type->integer.bits, decl_enum->type->integer.is_signed))
        {
            sema_error_at(&value->span, "Enum value '%.*s' exceeds the type size", SPLAT_TOK(entry->name));
            success = false;
            is_first_value = false;
            continue;

            if (!is_first_value && bigint_cmp(&current_value, &value->const_expr.value.big_int) != INT_LT)
            {
                sema_error_at(&value->span, "Enum values must be placed in strictly ascending value order");
                success = false;
            }
            bigint_init_bigint(&current_value, &value->const_expr.value.big_int);
            bigint_incr(&current_value);
            is_first_value = false;
        }
        return success;
    }
}

static bool analyse_struct_names(Decl *type, bool new_namespace)
{
    // Clear the table
    if (new_namespace)
    {
        table_clear(&analyser->temp_table);
    }

    bool success = true;
    Vector *members = type->structure.members;
    for (unsigned  i = 0; i < members->size; i++)
    {
        Decl *member = members->entries[i];
        Token *name = &member->name;
        if (name->length == 0)
        {
            assert(member->type_id == DECL_STRUCT_TYPE);
            success = analyse_struct_names(member, false) && success;
            continue;
        }
        Decl *old_member = table_set_token(&analyser->temp_table, name, member);
        if (old_member)
        {
            sema_error_at(name, "Duplicate member '%.*s'", SPLAT_TOK(*name));
            prev_at(&old_member->span, "Previous definition was here");
            success = false;
            continue;
        }
    }
    if (!success) return false;

    // Now we need to handle the substructs:
    for (unsigned  i = 0; i < members->size; i++)
    {
        Decl *member = members->entries[i];
        if (member->name.length == 0) continue;
        if (member->type_id != DECL_STRUCT_TYPE) continue;
        success = analyse_struct_names(member, true) &success;
    }
    return success;
}

bool analyse_type(Type *type, bool is_public)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
            return false;
        case TYPE_VOID:
        case TYPE_NIL:
        case TYPE_BOOL:
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
            return true;
        case TYPE_POINTER:
            return analyse_type(type->pointer.base, is_public);
        case TYPE_ARRAY:
            return analyse_type(type->array.base, is_public);
        case TYPE_STRUCT:
        case TYPE_ENUM:
        case TYPE_FUNC:
        case TYPE_UNION:
            // Checked later
            return true;
        case TYPE_UNRESOLVED:
            return analyse_unresolved_type(type, is_public);
        case TYPE_OPAQUE:
            return analyse_type(type->opaque.base, is_public);
        case TYPE_IMPORT:
            FATAL_ERROR("Should never occur");
    }
    FATAL_ERROR("Unreachable");
}
#ifdef TODO
static bool decl_resolve_type(Decl *decl)
{
    assert(!decl->is_resolved);
    Type *type = resolve_type(decl->type_expr, decl->is_public);
    if (!type) return false;
    decl->type = type;
    decl->is_resolved = true;
    return false;
}
#endif

static inline bool analyse_type_decl(Decl *decl)
{
    LOG_FUNC;

    // check generic type

    bool success = analyse_type(decl->type, decl->is_public);

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
        case DECL_ALIAS_TYPE:
            if (success)
            {
                // TODO investigate
                /*
                AliasType* A = cast<AliasType>(D->getType().getTypePtr());
                QualType Q = TR->resolveUnresolved(A->getRefType());
                A->updateRefType(Q);
                */
            }
            break;
        case DECL_STRUCT_TYPE:
            analyse_struct_names(decl, true);
            break;
        case DECL_ENUM_TYPE:
            if (!decl->enumeration.constants->size)
            {
                sema_error_at(&decl->name, "No empty enum '%.*s'", SPLAT_TOK(decl->name));
                success = false;
            }
            break;
        case DECL_FUNC_TYPE:
            // TODO investivate
            decl->func_type.func_decl->module = decl->module;
            // dont check return/argument types yet
            break;
    }
    return success;
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
    table_clear(&analyser->temp_table);

    bool success = true;
    for (unsigned a = 0; a < attributes->size; a++)
    {
        Ast *attribute = attributes->entries[a];
        assert(attribute->type == AST_ATTRIBUTE);
        Token *name = &attribute->attribute.name;
        Ast *old = table_set_token(&analyser->temp_table, &attribute->attribute.name, attribute);
        if (old)
        {
            sema_error_at(&attribute->span, "Attribute %.*s appears more than once in attribute list", SPLAT_TOK(*name));
            success = false;
            continue;
        }
        AttributeType attribute_type = attribute_type_from_token(name);
        switch (attribute_type)
        {
            case ATTRIBUTE_UNKNOWN:
                break;
            case ATTRIBUTE_EXPORT:
                if (!decl->is_public)
                {
                    sema_error_at(&attribute->span, "Cannot use export on non-public declaration");
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
                    sema_error_at(&attribute->span, "@packed can only be used with structs");
                    success = false;
                }
            case ATTRIBUTE_UNUSED:
            case ATTRIBUTE_UNUSED_PARAMS:
                break;
            case ATTRIBUTE_SECTION:
                if (CONST_FULL != evaluate_constant(attribute->attribute.value)
                    || attribute->attribute.value->const_expr.value.type != VALUE_TYPE_STRING)
                {
                    sema_error_at(&attribute->attribute.value->span, "Expected a string argument for @section");
                    success = false;
                    break;
                }
                if (attribute->attribute.value->const_expr.value.str_len == 0)
                {
                    sema_error_at(&attribute->span, "A non empty string is required for @section");
                    success = false;
                    break;
                }
                break;
            case ATTRIBUTE_INLINE:
            case ATTRIBUTE_NORETURN:
                // TODO errors
                break;
            case ATTRIBUTE_ALIGNED:
                if (CONST_FULL != evaluate_constant(attribute->attribute.value)
                    || attribute->attribute.value->const_expr.value.type != VALUE_TYPE_INT)
                {
                    sema_error_at(&attribute->attribute.value->span, "Expected a integer argument for @aligned");
                    success = false;
                    break;
                }
                if (attribute->attribute.value->const_expr.value.big_int.is_negative)
                {
                    sema_error_at(&attribute->attribute.value->span, "Expected a positive integer for @aligned");
                    success = false;
                    break;
                }
                if (bigint_popcount_unsigned(&attribute->attribute.value->const_expr.value.big_int) != 1)
                {
                    sema_error_at(&attribute->attribute.value->span, "@aligned must be a power of 2");
                    success = false;
                    // TODO check if alignment is too small (smaller then size of type)
                    break;
                }
                break;
            case ATTRIBUTE_WEAK:
                if (!decl->is_public || !decl->is_exported)
                {
                    // TODO ordering?!
                    sema_error_at(&attribute->span, "@weak can only be used for public, exported declarations");
                    success = false;
                    break;
                }
                break;
            case ATTRIBUTE_OPAQUE:
                if (decl->type_id != DECL_STRUCT_TYPE)
                {
                    sema_error_at(&attribute->span, "@opaque can only be used with structs");
                    success = false;
                    break;
                }
                if (!decl->is_public)
                {
                    sema_error_at(&attribute->span, "@opaque can only be used public structs");
                    success = false;
                    break;
                }
                break;
            case ATTRIBUTE_CNAME:
                if (!analyser->parser->is_interface)
                {
                    sema_error_at(&attribute->span, "@cname can only be used with interfaces");
                    success = false;
                    break;
                }
                decl->has_cname = true;
                break;
            case ATTRIBUTE_NO_TYPEDEF:
                if (decl->type_id != DECL_STRUCT_TYPE)
                {
                    sema_error_at(&attribute->span, "@no_typedef can only be used with structs");
                    success = false;
                    break;
                }
                if (!analyser->parser->is_interface)
                {
                    sema_error_at(&attribute->span, "@no_typedef can only be used with interfaces");
                    success = false;
                    break;
                }
                decl->structure.no_typedef = true;
                break;
        }
    }
    return success;
}

void resolve_types(void)
{
    for (unsigned i = 0; i < analyser->parser->types->size; i++)
    {
        Decl *decl = analyser->parser->types->entries[i];
        analyse_type_decl(decl);
        analyse_attributes(decl);
    }
}
