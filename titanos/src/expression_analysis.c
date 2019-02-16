//
// Created by Christoffer Lernö on 2019-02-14.
//

#include "expression_analysis.h"
#include "constant_folding.h"
#include "error.h"
#include "type_analysis.h"
#include "diagnostics.h"
#include "builtins.h"

static bool analyse_cast_expr(Expr *expr);

static CastResult perform_compile_time_cast(Expr *expr, Type *target_type, bool implicit);


static inline bool is_castable(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
            return false;
        case TYPE_UNRESOLVED:
        case TYPE_IMPORT:
            UNREACHABLE;
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_VOID:
        case TYPE_NIL:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_DECLARED:
        case TYPE_TYPEVAL:
        case TYPE_BUILTIN:
            return true;
    }
}

/**
 * Try to perform compile time cast on expression, if it fails, insert a cast.
 *
 * @param expr the expression to cast
 * @param type the type to cast to
 * @param implicit if the conversion is implicit or forced
 * @return false if an error was encountered
 */
bool insert_cast_if_needed(Expr *expr, Type *type, bool implicit)
{
    assert(expr->type);
    assert(type);
    if (expr->type == type) return true;
    CastResult result = perform_compile_time_cast(expr, type, implicit);
    switch (result)
    {
        case CAST_INLINE:
            return true;
        case CAST_FAILED:
            return false;
        default:
            break;
    }
    Expr *copy = expr_copy(expr);
    expr->expr_id = EXPR_CAST;
    expr->type = type;
    expr->cast_expr.type = type;
    expr->cast_expr.expr = copy;
    expr->cast_expr.cast_result = result;
    return true;
}

bool insert_implicit_cast_if_needed(Expr *expr, Type *type)
{
    return insert_cast_if_needed(expr, type, true);
}

bool analyse_init_expr(Decl *decl)
{
    if (!analyse_expr(decl->var.init_expr, RHS)) return false;
    if (!insert_implicit_cast_if_needed(decl->var.init_expr, decl->var.type))
    {
        char *type_name = type_to_string(decl->var.type);
        sema_error_at(&decl->span, "Cannot implictly cast expression to '%s'", type_name);
        return false;
    }
    return true;
}


typedef Value (*BinOp)(Value, Value);

Type *try_upcasting_for_pointer_arithmetics(Expr *left, Expr *right)
{
    Type *left_type = left->type;
    Type *right_type = right->type;

    if (left_type->type_id == TYPE_NIL && right_type->type_id == TYPE_NIL) return NULL;
    if (left_type->type_id != TYPE_POINTER && right_type->type_id != TYPE_POINTER) return NULL;

    if (left_type->type_id == TYPE_POINTER && right_type->type_id == TYPE_POINTER)
    {
        return type_is_same(left_type->pointer.base, right_type->pointer.base) ? left_type : NULL;
    }

    Expr *non_pointer_expr = left_type->type_id == TYPE_POINTER ? right : left;
    Expr *pointer_expr = left_type->type_id == TYPE_POINTER ? left : right;
    Type *non_pointer_type = non_pointer_expr->type;

    if (non_pointer_type->type_id == TYPE_NIL) return pointer_expr->type;

    if (non_pointer_type->type_id != TYPE_BUILTIN) return NULL;

    switch (non_pointer_type->builtin.builtin_id)
    {
        case BUILTIN_FLOAT:
            return NULL;
        case BUILTIN_SIGNED_INT:
        case BUILTIN_UNSIGNED_INT:
            return pointer_expr->type;
        case BUILTIN_BOOL:
            return insert_implicit_cast_if_needed(non_pointer_expr, type_builtin_u8()) ? pointer_expr->type : NULL;
    }
}

bool try_upcasting_binary_for_pointer_arithmetics(Expr *binary_expr)
{
    Type *type = try_upcasting_for_pointer_arithmetics(binary_expr->binary_expr.left, binary_expr->binary_expr.right);
    if (type)
    {
        binary_expr->type = type;
        return true;
    }
    return false;
}

bool is_arithmetics_type(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_IMPORT:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_DECLARED:
        case TYPE_TYPEVAL:
            return false;
        case TYPE_NIL:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_BUILTIN:
            return true;
    }
}

bool is_real_arithmetics_type(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_IMPORT:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_DECLARED:
        case TYPE_TYPEVAL:
            return false;
        case TYPE_BUILTIN:
            return type->builtin.builtin_id != BUILTIN_BOOL;
        case TYPE_NIL:
            return false;
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
            return true;
    }
}

static inline Type *try_upcasting_for_arithmetics(Expr *left, Expr *right)
{
    Type *left_type = left->type;
    Type *right_type = right->type;

    if (!is_arithmetics_type(left_type) || !is_arithmetics_type(right_type)) return NULL;

    if (type_is_same(left_type, right_type)) return left_type;

    bool reverse_order = false;
    if (left_type->type_id > right_type->type_id)
    {
        reverse_order = true;
    }
    else if (right_type->type_id == left_type->type_id && left_type->type_id == TYPE_BUILTIN)
    {
        int bit_diff = (int)left_type->builtin.bits - (int)right_type->builtin.bits;
        switch (left_type->builtin.builtin_id)
        {
            case BUILTIN_FLOAT:
                reverse_order = right_type->builtin.builtin_id == BUILTIN_FLOAT && bit_diff < 0;
                break;
            case BUILTIN_UNSIGNED_INT:
                reverse_order = right_type->builtin.builtin_id == BUILTIN_FLOAT || bit_diff < 0;
                break;
            case BUILTIN_SIGNED_INT:
                reverse_order = right_type->builtin.builtin_id == BUILTIN_FLOAT || bit_diff <= 0;
                break;
            case BUILTIN_BOOL:
                reverse_order = right_type->builtin.builtin_id != BUILTIN_BOOL;
                break;
        }
    }

    if (!is_real_arithmetics_type(left_type) && !is_real_arithmetics_type(right_type))
    {
        if (!insert_implicit_cast_if_needed(left, type_builtin_i32()) &&
                insert_implicit_cast_if_needed(right, type_builtin_i32())) return NULL;
        return type_builtin_i32();
    }
    if (reverse_order) return insert_implicit_cast_if_needed(left, right_type) ? right_type : NULL;
    return insert_implicit_cast_if_needed(right, left_type) ? left_type : NULL;
}

static inline bool try_upcasting_binary_for_arithmetics(Expr *binary_expr)
{
    Type *type = try_upcasting_for_arithmetics(binary_expr->binary_expr.left, binary_expr->binary_expr.right);
    if (type)
    {
        binary_expr->type = type;
        return true;
    }
    return false;
}

static inline bool is_both_const(Expr *left, Expr *right)
{
    return left->expr_id == EXPR_CONST && right->expr_id == EXPR_CONST;
}


static inline bool analyse_minus_expr(Expr *binary)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;
    // TODO improve
    if (try_upcasting_binary_for_pointer_arithmetics(binary))
    {
        return true;
    }
    if (!try_upcasting_for_arithmetics(left, right))
    {
        sema_error_at(&binary->span, "Can't subtract '%s' from '%s'", type_to_string(right->type), type_to_string(left->type));
        return false;
    }
    if (is_both_const(left, right))
    {
        binary->const_expr.value = value_sub(left->const_expr.value, right->const_expr.value);
        // TODO handle error
        binary->expr_id = EXPR_CONST;
    }
    return true;
}


static inline bool analyse_plus_expr(Expr *binary)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;
    // TODO improve
    if (try_upcasting_binary_for_pointer_arithmetics(binary))
    {
        return true;
    }
    if (!try_upcasting_binary_for_arithmetics(binary))
    {
        sema_error_at(&binary->span, "Can't add '%s' to '%s'", type_to_string(right->type), type_to_string(left->type));
        return false;
    }
    if (is_both_const(left, right))
    {
        binary->const_expr.value = value_add(left->const_expr.value, right->const_expr.value);
        // TODO handle error
        binary->expr_id = EXPR_CONST;
    }
    return true;
}

static inline bool analyse_mult_expr(Expr *binary)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;
    if (!try_upcasting_binary_for_arithmetics(binary))
    {
        sema_error_at(&binary->span, "Can't multiply '%s' by '%s'", type_to_string(left->type), type_to_string(right->type));
        return false;
    }
    if (is_both_const(left, right))
    {
        binary->const_expr.value = value_mult(left->const_expr.value, right->const_expr.value);
        // TODO handle error
        binary->expr_id = EXPR_CONST;
    }
    return true;
}

bool analyse_binary_expr(Expr *expr, Side side)
{
    LOG_FUNC
    Expr *left = expr->binary_expr.left;
    Expr *right = expr->binary_expr.right;
    bool success = analyse_expr(left, RHS);
    success = analyse_expr(right, RHS) & success;
    if (!success) return false;

    switch (expr->binary_expr.operator)
    {
        case TOKEN_EQ:
            // Make sure types match, otherwise try inserting a cast.
            return insert_implicit_cast_if_needed(right, left->type);
        case TOKEN_MINUS:
            return analyse_minus_expr(expr);
        case TOKEN_PLUS:
            return analyse_plus_expr(expr);
        case TOKEN_MOD:
            return analyse_mult_expr(expr);
        case TOKEN_DIV:
        case TOKEN_STAR:
        case TOKEN_EQEQ:
        case TOKEN_NOT_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQ:
        case TOKEN_LESS:
        case TOKEN_LESS_EQ:
        case TOKEN_LEFT_SHIFT:
        case TOKEN_RIGHT_SHIFT:
        case TOKEN_OR:
        case TOKEN_BIT_OR:
        case TOKEN_AND:
        case TOKEN_AMP:
        case TOKEN_BIT_XOR:
        case TOKEN_PLUS_ASSIGN:
        case TOKEN_MINUS_ASSIGN:
        case TOKEN_MULT_ASSIGN:
        case TOKEN_MOD_ASSIGN:
        case TOKEN_DIV_ASSIGN:
        case TOKEN_AND_ASSIGN:
        case TOKEN_BIT_AND_ASSIGN:
        case TOKEN_OR_ASSIGN:
        case TOKEN_BIT_OR_ASSIGN:
        case TOKEN_BIT_XOR_ASSIGN:
        case TOKEN_RIGHT_SHIFT_ASSIGN:
        case TOKEN_LEFT_SHIFT_ASSIGN:
            FATAL_ERROR("TODO");
            break;
        default:
            FATAL_ERROR("Not possible");
    }
    return expr->type != NULL;
}


static inline bool insert_bool_cast_for_conditional_if_needed(Expr *expr)
{
    // TODO warn on if (a = foo)
    return insert_cast_if_needed(expr, type_builtin_bool(), false);
}

static inline bool analyse_ternary_expr(Expr *expr, Side side)
{
    Expr *cond = expr->ternary_expr.cond;
    Expr *then_expr = expr->ternary_expr.then_expr;
    Expr *else_expr = expr->ternary_expr.else_expr;
    if (!analyse_expr(cond, RHS)) return false;
    if (!analyse_expr(then_expr, side)) return false;
    if (!analyse_expr(else_expr, side)) return false;
    if (!insert_bool_cast_for_conditional_if_needed(cond)) return false;

    if (!type_is_same(then_expr->type, else_expr->type))
    {
        Type *type = try_upcasting_for_arithmetics(then_expr, else_expr);
        if (!type)
        {
            char *t1 = type_to_string(else_expr->type);
            char *t2 = type_to_string(then_expr->type);
            sema_error_at(&expr->span, "Ternary expression result has incompatible types '%s' and '%s'", t1, t2);
            return false;
        }
        expr->type = type;
    }
    else
    {
        expr->type = then_expr->type;
    }
    if (cond->expr_id == EXPR_CONST)
    {
        assert(cond->const_expr.value.type == VALUE_TYPE_BOOL);
        if (cond->const_expr.value.b)
        {
            expr_replace(expr, then_expr);
        }
        else
        {
            expr_replace(expr, else_expr);
        }
    }
    return true;
}


static bool is_assignable(Expr *expr)
{
    return true;
    /*
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            break;
        case EXPR_CONST:
            break;
        case EXPR_BINARY:
            return is_assignable(expr->binary_expr.left) && is_assignable(expr->binary_expr.right);
        case EXPR_TERNARY:
            return returnis_assignable(expr->ternary_expr.true_expr) && is_assignable(expr->ternary_expr.false_expr);
        case EXPR_UNARY:
            // &x = ... and *x = ... are valid, the rest (-, --, ++, !, ~) are not.
            return expr->unary_expr.operator == TOKEN_STAR || expr->unary_expr.operator == TOKEN_AMP;
            break;
        case EXPR_POST:break;
        case EXPR_IDENTIFIER:break;
        case EXPR_CALL:break;
        case EXPR_SIZEOF:break;
        case EXPR_CAST:break;
        case EXPR_SUBSCRIPT:break;
        case EXPR_ACCESS:break;
        case EXPR_STRUCT_INIT_VALUES:break;
        case EXPR_DESIGNATED_INITIALIZED:break;
    }
    bool FunctionAnalyser::checkAssignee(Expr* expr) const {
        switch (expr->getKind()) {
            case EXPR_INTEGER_LITERAL:
            case EXPR_FLOAT_LITERAL:
            case EXPR_BOOL_LITERAL:
            case EXPR_CHAR_LITERAL:
            case EXPR_STRING_LITERAL:
            case EXPR_NIL:
                break;
            case EXPR_CALL:
            case EXPR_IDENTIFIER:
                // ok
                return true;
            case EXPR_INITLIST:
            case EXPR_DESIGNATOR_INIT:
            case EXPR_TYPE:
                break;
            case EXPR_BINOP:
                // ok
                return true;
            case EXPR_CONDOP:
                break;
            case EXPR_UNARYOP:
                // sometimes... (&)
                return true;
            case EXPR_BUILTIN:
                break;
            case EXPR_ARRAYSUBSCRIPT:
            case EXPR_MEMBER:
            case EXPR_PAREN:
            case EXPR_BITOFFSET:
                // ok
                return true;
            case EXPR_CAST:
                TODO;
                break;
        }
        // expr is not assignable
        // TODO test (also ternary)
        Diag(expr->getLocation(), diag::err_typecheck_expression_not_modifiable_lvalue);
        return false;
    }
*/
}

static bool cast_const_type_expression(Expr *expr, Type *target_type, bool is_implicit)
{
    switch (expr->expr_id)
    {
        case EXPR_TERNARY:
            if (!cast_const_type_expression(expr->ternary_expr.then_expr, target_type, is_implicit)) return false;
            if (!cast_const_type_expression(expr->ternary_expr.else_expr, target_type, is_implicit)) return false;
            return true;
        case EXPR_CONST:
            break;
        default:
            UNREACHABLE;
    }
    assert(expr->expr_id == EXPR_CONST);
    Value *value = &expr->const_expr.value;
    bool allow_trunc = !is_implicit;
    switch (target_type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_IMPORT:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_DECLARED:
        case TYPE_TYPEVAL:
        case TYPE_NIL:
            UNREACHABLE
        case TYPE_BUILTIN:
            switch (target_type->builtin.builtin_id)
            {
                case BUILTIN_FLOAT:
                    return value_convert(value, VALUE_TYPE_FLOAT, target_type->builtin.bits, false, allow_trunc);
                case BUILTIN_UNSIGNED_INT:
                    return value_convert(value, VALUE_TYPE_INT, target_type->builtin.bits, true, allow_trunc);
                case BUILTIN_SIGNED_INT:
                    return value_convert(value, VALUE_TYPE_INT, target_type->builtin.bits, false, allow_trunc);
                case BUILTIN_BOOL:
                    return value_convert(value, VALUE_TYPE_BOOL, target_type->builtin.bits, true, allow_trunc);
            }
            UNREACHABLE
            break;
        case TYPE_CONST_FLOAT:
            return value_convert(value, VALUE_TYPE_FLOAT, 0, false, allow_trunc);
        case TYPE_CONST_INT:
            return value_convert(value, VALUE_TYPE_INT, 0, false, allow_trunc);
    }
    UNREACHABLE
}

static CastResult perform_compile_time_cast(Expr *expr, Type *target_type, bool implicit)
{
    Type *source_type = expr->type;
    if (source_type == target_type)
    {
        return CAST_INLINE;
    }
    bool is_same_type = source_type->type_id == target_type->type_id;
    switch (target_type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
            // Since we resolved the type successfully
            UNREACHABLE
        case TYPE_IMPORT:
            // Can't occur here
            UNREACHABLE
        case TYPE_VOID:
        case TYPE_NIL:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_OPAQUE:
        case TYPE_TYPEVAL:
        case TYPE_ARRAY:
        {
            char *name = type_to_string(target_type);
            sema_error_at(&expr->span, "Cannot cast expression to '%s'", name);
            free(name);
            return CAST_FAILED;
        }
        case TYPE_STRING:
            if (is_same_type)
            {
                // Inline cast
                return CAST_INLINE;
            }
            // Default error
            break;
        case TYPE_POINTER:
            // Pointer and array types can be immediately converted.
            if (source_type->type_id == TYPE_POINTER || source_type->type_id == TYPE_ARRAY) return CAST_PTRPTR;
            // Explicit pointer to int conversions are fine.
            if (type_is_int(target_type) && !implicit) return CAST_INTPTR;
            // Default error
            break;
        case TYPE_DECLARED:
            // TODO enum!
            if (is_same_type && source_type->decl == target_type->decl)
            {
                // Inline cast
                expr->type = target_type;
                return CAST_INLINE;
            }
            break;
        case TYPE_BUILTIN:
            if (!is_same_type)
            {
                // Inline nil
                switch (source_type->type_id)
                {
                    case TYPE_NIL:
                    case TYPE_CONST_FLOAT:
                    case TYPE_CONST_INT:
                        if (!cast_const_type_expression(expr, target_type, implicit))
                        {
                            char *type_name = type_to_string(target_type);
                            sema_error_at(&expr->span, "Cannot implicitly cast '%.*s' to '%s'", expr, type_name);
                            free(type_name);
                            return CAST_FAILED;
                        }
                        expr->type = target_type;
                        return CAST_INLINE;
                    case TYPE_POINTER:
                    case TYPE_ARRAY:
                        // Any pointers can be cast to the non-float builtins.
                        if (!implicit && target_type->builtin.builtin_id != BUILTIN_FLOAT) return CAST_PTRINT;
                        break;
                    default:
                        break;
                }
                break;
            }
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                if (!cast_const_type_expression(expr, target_type, implicit))
                {
                    assert(expr->cast_expr.implicit);
                    char *type_name = type_to_string(expr->cast_expr.type);
                    sema_error_at(&expr->span, "Cannot implicitly cast '%.*s' to '%s'",
                                  expr->cast_expr.expr, type_name);
                    free(type_name);
                    return CAST_FAILED;
                }
                expr->type = target_type;
                return CAST_INLINE;
            }
            if (source_type->builtin.builtin_id == target_type->builtin.builtin_id &&
                source_type->builtin.bits == target_type->builtin.bits)
            {
                DEBUG_LOG("Same but different");
                return CAST_INLINE;
            }
            // Any cast between builtin types are ok (even though some means truncating)
            return builtin_casts[source_type->builtin.builtin_id][target_type->builtin.builtin_id];
    }
    char *target_name = type_to_string(target_type);
    char *source_name = type_to_string(source_type);
    sema_error_at(&expr->span, "Cannot cast '%s' to '%s'", source_name, target_name);
    free(target_name);
    free(source_name);
    return CAST_FAILED;
}

static bool analyse_cast_expr(Expr *expr)
{
    if (!analyse_expr(expr->cast_expr.expr, RHS)) return false;
    if (!resolve_type(&expr->cast_expr.type, false)) return false;
    Type *target_type = expr->cast_expr.type;
    Type *source_type = expr->cast_expr.expr->type;
    assert(target_type && source_type);
    expr->type = expr->cast_expr.type;
    expr->cast_expr.cast_result = perform_compile_time_cast(expr->cast_expr.expr, expr->cast_expr.type, expr->cast_expr.implicit);
    switch (expr->cast_expr.cast_result)
    {
        case CAST_FAILED:
            return false;
        case CAST_INLINE:
            expr_replace(expr, expr->cast_expr.expr);
            return true;
        default:
            return true;
    }
}

static bool analyse_type_expr(Expr *expr)
{
    Type *type = expr->type_expr.type;
    if (!resolve_type(&type, false)) return false;
    // Type now resolves, we can set the expression
    Type *type_of_type = new_type(TYPE_TYPEVAL, false);
    type_of_type->is_public = type->is_public;
    type_of_type->is_exported = type->is_exported;
    type_of_type->module = type->module;
    type_of_type->type_of_type = type;
    expr->type = type_of_type;
    return true;
}

static bool resolve_identifier(Expr *expr, Side side)
{
    LOG_FUNC
    Decl *decl = scope_find_symbol(&expr->identifier_expr.identifier, false, false);
    if (!decl)
    {
        return false;
    }
    expr->identifier_expr.resolved = decl;
    decl->is_used = true;
    switch (decl->type_id)
    {
        case DECL_FUNC:
        case DECL_ENUM_CONSTANT:
        case DECL_STRUCT_TYPE:
        case DECL_FUNC_TYPE:
        case DECL_BUILTIN:
        case DECL_ALIAS_TYPE:;
        case DECL_ENUM_TYPE:
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
            if (side == LHS)
            {
                sema_error_at(&expr->span, "Only variables can be assigned to");
                return false;
            }
            expr->type = &decl->type;
            expr->const_state = CONST_FULL;
            break;
        case DECL_VAR:
            if (decl->var.in_init)
            {
                sema_error_at(&expr->span, "Used '%.*s' in own initialization", SPLAT_TOK(expr->identifier_expr.identifier));
                return false;
            }
            if (side == LHS && decl->var.type->is_const)
            {
                sema_error_at(&expr->span, "Cannot assign to constant value");
                return false;
            }
            expr->const_state = decl->var.type->is_const ? CONST_FULL : CONST_NONE;
            expr->type = decl->var.type;
            break;
    }
    if (side == RHS)
    {
        decl->is_used = true;
        // TODO
        // if (usedPublicly) decl->is_used_public = true;
    }
    return expr->const_state;
}

bool analyse_const_expr(Expr *expr)
{
    assert(expr->const_state == CONST_FULL);
    assert(expr->type != NULL);
    return true;
}

bool analyse_expr(Expr *expr, Side side)
{
    LOG_FUNC
    if (side == LHS)
    {
        if (!is_assignable(expr)) return false;
    }
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            return analyse_type_expr(expr);
        case EXPR_CAST:
            return analyse_cast_expr(expr);
        case EXPR_CONST:
            return analyse_const_expr(expr);
        case EXPR_BINARY:
            return analyse_binary_expr(expr, side);
        case EXPR_TERNARY:
            return analyse_ternary_expr(expr, side);
        case EXPR_UNARY:
            //return analyse_unary_expr(expr, side);
        case EXPR_POST:
            //return analyse_post_expr(expr, side);
        case EXPR_IDENTIFIER:
            return resolve_identifier(expr, side);
        case EXPR_CALL:
            //return analyse_call(expr, side);
        case EXPR_SIZEOF:
            //return analyse_sizeof(expr, side);
        case EXPR_SUBSCRIPT:
            //return analyse_subscript(expr, side);
        case EXPR_ACCESS:
            //return analyse_access(expr, side);
        case EXPR_STRUCT_INIT_VALUES:
            //return analyse_struct_init_values(expr, side);
        case EXPR_DESIGNATED_INITIALIZER:
            //return analyse_designated_initializer(expr, side);
            break;
    }
    FATAL_ERROR("Unreachable");
}
