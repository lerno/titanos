//
// Created by Christoffer Lernö on 2019-02-14.
//

#include "expression_analysis.h"
#include "error.h"
#include "type_analysis.h"
#include "diagnostics.h"
#include "builtins.h"
#include "expr.h"
#include "printer.h"

static bool analyse_cast_expr(Expr *expr, Side side);

static CastType perform_compile_time_cast(Expr *expr, TypeOld *target_type, bool implicit);

bool default_function_array_conversion(Expr *expr)
{
    // Or func type??
    QualifiedType type = type_canonical(expr->type);
    if (type.type->type_id == TYPE_FUNC)
    {
// TODO        return insert_implicit_cast_if_needed(expr, type_new_pointer(expr->type));
    }
    if (type.type->type_id == TYPE_ARRAY)
    {
// TODO        return insert_implicit_cast_if_needed(expr, type_new_pointer(expr->type->array.base));
    }
    return true;
}

bool check_null_deref(Expr *expr)
{
    if (expr->expr_id != EXPR_UNARY) return true;
    if (expr->unary_expr.operator != UNARYOP_DEREF) return true;
    if (expr->unary_expr.expr->expr_id != EXPR_CONST) return true;
    if (expr->unary_expr.expr->const_expr.value.type == VALUE_TYPE_NIL)
    {
        sema_error_at(expr->span, "Cannot deref nil type");
        return false;
    }
    return true;
}

bool default_lvalue_conversion(Expr *expr)
{
    if (!check_null_deref(expr)) return false;

    QualifiedType qt = type_canonical(expr->type);

    if (qt.type->type_id == TYPE_VOID) return true;

    if (qt.qualifier)
    {
        expr->type = type_remove_qualifier(expr->type);
    }

    expr->is_lvalue = false;
    return true;
}

bool default_function_array_lvalue_conversion(Expr *expr)
{
    if (!default_function_array_conversion(expr)) return false;
    return default_lvalue_conversion(expr);
}


static bool perform_unary_conversions(Expr *expr)
{
    if (!default_lvalue_conversion(expr)) return false;
    QualifiedType qt = type_canonical(expr->type);
    if (type_is_int(qt.type))
    {
        // TODO handle bitfield
        return true;
    }
    if (qt.type->type_id == TYPE_ENUM)
    {
        // Don't worry about the qualifiers, the enum type cannot have one.
        expr->type.type = qt.type->decl->enum_decl.type.type;
    }
    return true;
}

static bool cast_const_type_expression(Expr *expr, CastType cast_type, QualifiedType type, bool implicit)
{
    assert(expr->expr_id == EXPR_CONST);
    Value *valref = &expr->const_expr.value;
    switch (cast_type)
    {
        case CAST_INLINE:
        case CAST_FAILED:
        case CAST_PTRPTR:
        case CAST_INTPTR:
            UNREACHABLE
        case CAST_PTRINT:
            assert(valref->type == VALUE_TYPE_NIL);
            valref->big_int.digit = 0;
            valref->big_int.digits = 0;
            valref->big_int.is_negative = false;
            valref->int_bits = type_bits(type.type);
            valref->type = VALUE_TYPE_INT;
            break;
        case CAST_FPFP:
            if (!value_convert(valref, VALUE_TYPE_FLOAT, type_bits(type.type), false, !implicit))
            {
                UNREACHABLE
            }
            break;
        case CAST_FPUI:
            if (implicit && valref->f < 0)
            {
                sema_error_at(expr->span, "Cannot fit negative %f constant into %s", valref->f, type_to_string(type));
                return false;
            }
            if (!value_convert(valref, VALUE_TYPE_INT, type_bits(type.type), true, !implicit))
            {
                sema_error_at(expr->span, "Cannot fit %f into %s", valref->f, type_to_string(type));
                return false;
            }
            break;
        case CAST_FPSI:
            if (!value_convert(valref, VALUE_TYPE_INT, type_bits(type.type), false, !implicit))
            {
                sema_error_at(expr->span, "Cannot fit %f into %s", valref->f, type_to_string(type));
                return false;
            }
            break;
        case CAST_UIFP:
            if (!value_convert(valref, VALUE_TYPE_FLOAT, type_bits(type.type), true, !implicit)) return false;
            break;
        case CAST_SIFP:
            if (!value_convert(valref, VALUE_TYPE_FLOAT, type_bits(type.type), true, !implicit)) return false;
            break;
        case CAST_SIUI:
        case CAST_UIUI:
            if (!value_convert(valref, VALUE_TYPE_INT, type_bits(type.type), true, !implicit))
            {
                // Should never happen because we now prohibit this type of casts.
                UNREACHABLE;
            }
            break;
        case CAST_UISI:
        case CAST_SISI:
            if (!value_convert(valref, VALUE_TYPE_INT, type_bits(type.type), false, !implicit))
            {
                // Should never happen because we now prohibit this type of casts.
                UNREACHABLE;
            }
            break;
        case CAST_FPBOOL:
        case CAST_INTBOOL:
            if (!value_convert(valref, VALUE_TYPE_BOOL, 1, true, true))
            {
                UNREACHABLE
            }
            break;
        case CAST_BOOLFP:
            if (!value_convert(valref, VALUE_TYPE_FLOAT, type_bits(type.type), false, !implicit))
            {
                UNREACHABLE
            }
            break;
        case CAST_BOOLINT:
            if (!value_convert(valref, VALUE_TYPE_INT, type_bits(type.type), type_is_signed(type.type), !implicit))
            {
                UNREACHABLE
            }
            break;
    }
    expr->type = type;
    return true;
}

static bool insert_cast(Expr *expr, CastType cast_type, QualifiedType type)
{
    if (expr->expr_id == EXPR_CONST)
    {
        return cast_const_type_expression(expr, cast_type, type, true);
    }

    Expr *copy = expr_copy(expr);
    expr->expr_id = EXPR_CAST;
    expr->type = type;
    expr->cast_expr.expr = copy;
    expr->cast_expr.cast_result = cast_type;
    return true;
}

static inline bool is_both_aritmetic(Type *left, Type *right)
{
    return type_is_aritmetic(left) && type_is_aritmetic(right);
}

bool perform_assignment_conversions(QualifiedType left_type, Expr *RHS)
{
    if (!perform_unary_conversions(RHS)) return false;
    QualifiedType right = type_remove_qualifier(type_canonical(RHS->type));
    QualifiedType left = type_remove_qualifier(type_canonical(left_type));
    if (type_is_same(left.type, right.type)) return true; // or left == right
    switch (left.type->type_id)
    {
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
            UNREACHABLE
        case TYPE_F32:
        case TYPE_F64:
            if (type_is_float(right.type))
            {
                if (type_bits(right.type) > type_bits(left.type))
                {
                    // Lossy? Warn? TODO
                }
                return insert_cast(RHS, CAST_FPFP, left);
            }
            if (type_is_int(right.type))
            {
                return insert_cast(RHS, type_is_signed(right.type) ? CAST_SIFP : CAST_FPFP, left);
            }
            if (type_is_bool(right.type))
            {
                return insert_cast(RHS, CAST_BOOLFP, left);
            }
            break;
        case TYPE_U64:
        case TYPE_U32:
        case TYPE_U16:
        case TYPE_U8:
            if (type_is_int(right.type))
            {
                if (type_bits(right.type) > type_bits(left.type)) break;
                return insert_cast(RHS, type_is_signed(right.type) ? CAST_SIUI : CAST_UIUI, left);
            }
            if (type_is_bool(right.type))
            {
                return insert_cast(RHS, CAST_BOOLINT, left);
            }
            break;
        case TYPE_I64:
        case TYPE_I32:
        case TYPE_I16:
        case TYPE_I8:
            if (type_is_int(right.type))
            {
                if (type_bits(right.type) > type_bits(left.type)) break;
                return insert_cast(RHS, type_is_signed(right.type) ? CAST_SIUI : CAST_UIUI, left);
            }
            if (type_is_bool(right.type))
            {
                return insert_cast(RHS, CAST_BOOLINT, left);
            }
            break;
        case TYPE_BOOL:
        case TYPE_NIL:
        case TYPE_POINTER:
        case TYPE_STRING:
        case TYPE_ARRAY:
        case TYPE_FUNC_TYPE:
        case TYPE_ENUM:
        case TYPE_FUNC:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_VOID:
            break;
        case TYPE_OPAQUE:
        case TYPE_ALIAS:
            UNREACHABLE
    }
    sema_error_at(RHS->span, "Cannot implicitly convert '%s' to '%s'", type_to_string(RHS->type), left_type);
    return false;
}

bool perform_arithmetic_conversions(Expr *LHS, Expr *RHS, bool is_compile_assign, bool allow_sign_conversion)
{
    if (!is_compile_assign && !perform_unary_conversions(LHS)) return false;
    if (!perform_unary_conversions(RHS)) return false;
    QualifiedType left = type_remove_qualifier(type_canonical(LHS->type));
    QualifiedType right = type_remove_qualifier(type_canonical(RHS->type));

    if (type_is_same(left.type, right.type)) return true; // or left == right

    if (!type_is_aritmetic(left.type) || !type_is_aritmetic(right.type)) return false;


    bool ordered = left.type->type_id <= right.type->type_id;
    QualifiedType *a = ordered ? &left : &right;
    QualifiedType *b = ordered ? &right : &left;

    Expr *converted = ordered ? RHS : LHS;

    switch (a->type->type_id)
    {
        case TYPE_F64:
        case TYPE_F32:
        case TYPE_CONST_FLOAT:
            if (type_is_float(b->type))
            {
                return insert_cast(converted, CAST_FPFP, *a);
            }
            if (type_is_ptr(b->type))
            {
                sema_error_at(converted->span, "Cannot convert pointer to floating point");
                return false;
            }
            if (type_is_bool(b->type))
            {
                return insert_cast(converted, CAST_FPBOOL, *a);
            }
            return insert_cast(converted, type_is_signed(b->type) ? CAST_SIFP : CAST_UIFP, *a);
        case TYPE_U64:
        case TYPE_U32:
        case TYPE_U16:
        case TYPE_U8:
            if (type_is_ptr(b->type))
            {
                return insert_cast(converted, CAST_PTRINT, *a);
            }
            if (type_is_signed(b->type))
            {
                if (!allow_sign_conversion)
                {
                    sema_error_at(converted->span, "Cannot implicitly convert this to %s", type_to_string(*a));
                    return false;
                }
                return insert_cast(converted, CAST_UISI, *a);
            }
            if (type_is_bool(b->type))
            {
                return insert_cast(converted, CAST_BOOLINT, *a);
            }
            return insert_cast(converted, CAST_UIUI, *a);
        case TYPE_I64:
        case TYPE_I32:
        case TYPE_I16:
        case TYPE_I8:
        case TYPE_BOOL:
        case TYPE_CONST_INT:
            if (type_is_ptr(b->type))
            {
                return insert_cast(converted, CAST_PTRINT, *a);
            }
            if (type_is_bool(b->type))
            {
                return insert_cast(converted, CAST_BOOLINT, *a);
            }
            return insert_cast(converted, type_is_signed(b->type) ? CAST_SISI : CAST_SIUI, *a);
        case TYPE_NIL:
        case TYPE_POINTER:
        case TYPE_STRING:
        case TYPE_ARRAY:
        case TYPE_ALIAS:
        case TYPE_FUNC_TYPE:
        case TYPE_ENUM:
        case TYPE_FUNC:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_VOID:
        case TYPE_OPAQUE:
            UNREACHABLE
    }
    UNREACHABLE
}


#ifdef OLD
static bool is_rvalue(Expr *expr)
{
    if (expr->is_lvalue)
    {
        sema_error_at(expr->span, "Expression is not assignable");
        return false;
    }
    return true;
}


/**
 * Try to perform compile time cast on expression, if it fails, insert a cast.
 *
 * @param expr the expression to cast
 * @param type the type to cast to
 * @param implicit if the conversion is implicit or forced
 * @return false if an error was encountered
 */
bool insert_cast_if_needed(Expr *expr, TypeOld *type, bool implicit)
{
    assert(!IS_INVALID(expr->type));
    assert(type);
    if (type_is_same(expr->type.type, type)) return true;
    TypeOld *target_type = type_unfold_non_opaque(type);
    CastResult result = perform_compile_time_cast(expr, target_type, implicit);
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
    expr->type.type = target_type;
    expr->cast_expr.type = target_type;
    expr->cast_expr.expr = copy;
    expr->cast_expr.cast_result = result;
    return true;
}

bool insert_implicit_cast_if_needed(Expr *expr, TypeOld *type)
{
    return insert_cast_if_needed(expr, type, true);
}

bool analyse_init_expr(Decl *decl)
{
    if (!analyse_expr(decl->var.init_expr, RHS)) return false;
    if (!insert_implicit_cast_if_needed(decl->var.init_expr, decl->type.type))
    {
        sema_error_at(decl->span, "Cannot implictly cast expression to '%s'", type_to_string(decl->type.type));
        return false;
    }
    return true;
}


typedef Value (*BinOpFunc)(Value, Value);

TypeOld *try_upcasting_for_pointer_arithmetics(Expr *left, Expr *right)
{
    return NULL;
#ifdef OLD
    TypeOld *left_type = left->type;
    TypeOld *right_type = right->type;

    if (left_type->type_id == XTYPE_NIL && right_type->type_id == XTYPE_NIL) return NULL;
    if (left_type->type_id != XTYPE_POINTER && right_type->type_id != XTYPE_POINTER) return NULL;

    if (left_type->type_id == XTYPE_POINTER && right_type->type_id == XTYPE_POINTER)
    {
        return type_is_same(left_type->pointer.base, right_type->pointer.base) ? left_type : NULL;
    }

    Expr *non_pointer_expr = left_type->type_id == XTYPE_POINTER ? right : left;
    Expr *pointer_expr = left_type->type_id == XTYPE_POINTER ? left : right;
    TypeOld *non_pointer_type = non_pointer_expr->type;

    if (non_pointer_type->type_id == XTYPE_NIL) return pointer_expr->type;

    switch (non_pointer_type->type_id)
    {
        case XTYPE_INT:
            return pointer_expr->type;
        case XTYPE_BOOL:
            return insert_implicit_cast_if_needed(non_pointer_expr, type_builtin_u8()) ? pointer_expr->type : NULL;
        default:
            return NULL;
    }
#endif
}

bool try_upcasting_binary_for_pointer_arithmetics(Expr *binary_expr)
{
    TypeOld *type = try_upcasting_for_pointer_arithmetics(binary_expr->binary_expr.left, binary_expr->binary_expr.right);
    if (type)
    {
        binary_expr->type = type;
        return true;
    }
    return false;
}


bool is_real_arithmetics_type(TypeOld *type)
{
    switch (type->type_id)
    {
        case XTYPE_INVALID:
        case XTYPE_UNRESOLVED:
        case XTYPE_VOID:
        case XTYPE_STRING:
        case XTYPE_OPAQUE:
        case XTYPE_POINTER:
        case XTYPE_ARRAY:
        case XTYPE_TYPEVAL:
        case XTYPE_BOOL:
        case XTYPE_NIL:
        case XTYPE_FUNC:
        case XTYPE_FUNC_TYPE:
        case XTYPE_STRUCT:
        case XTYPE_UNION:
        case XTYPE_RESOLVED:
        case XTYPE_ALIAS:
            return false;
        case XTYPE_INT:
        case XTYPE_FLOAT:
        case XTYPE_CONST_FLOAT:
        case XTYPE_CONST_INT:
        case XTYPE_ENUM:
            return true;
    }
}




static inline TypeOld *try_upcasting_for_arithmetics(Expr *left, Expr *right)
{
    TypeOld *left_type = type_unfold_non_opaque(left->type);
    TypeOld *right_type = type_unfold_non_opaque(right->type);

    if (!is_arithmetics_type(left_type) || !is_arithmetics_type(right_type)) return NULL;

    if (type_is_same(left_type, right_type)) return left_type;

    bool order = type_order(left_type, right_type);

    if (!is_real_arithmetics_type(left_type) && !is_real_arithmetics_type(right_type))
    {
        if (!insert_implicit_cast_if_needed(left, type_builtin_i32()) &&
                insert_implicit_cast_if_needed(right, type_builtin_i32())) return NULL;
        return type_builtin_i32();
    }
    if (order)
    {
        return insert_implicit_cast_if_needed(right, left_type) ? left_type : NULL;
    }
    else
    {
        return insert_implicit_cast_if_needed(left, right_type) ? right_type : NULL;
    }
}

static inline bool try_upcasting_binary_for_arithmetics(Expr *binary_expr)
{
    TypeOld *type = try_upcasting_for_arithmetics(binary_expr->binary_expr.left, binary_expr->binary_expr.right);
    if (type)
    {
        binary_expr->type = type;
        return true;
    }
    return false;
}



static inline bool analyse_minus_expr(Expr *binary, Side side)
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
        sema_error_at(binary->span, "Can't subtract '%s' from '%s'", type_to_string(right->type), type_to_string(left->type));
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


static inline bool analyse_plus_expr(Expr *binary, Side side)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;
    TypeOld *left_type = type_unfold_non_opaque(left->type);
    TypeOld *right_type = type_unfold_non_opaque(right->type);
    if (left_type->type_id == XTYPE_POINTER)
    {
        // TODO pointer int type
        TypeOld *target = type_is_signed2(right_type) ? type_builtin_i64() : type_builtin_u64();
        if (!insert_implicit_cast_if_needed(right, target))
        {
            sema_error_at(right->span, "Can't convert expression to a pointer int");
            return false;
        }
        return true;
    }
    if (!try_upcasting_binary_for_arithmetics(binary))
    {
        sema_error_at(binary->span, "Can't add '%s' to '%s'", type_to_string(right->type), type_to_string(left->type));
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
        sema_error_at(binary->span, "Can't multiply '%s' by '%s'", type_to_string(left->type), type_to_string(right->type));
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

static inline bool analyse_div_expr(Expr *binary)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;
    if (!try_upcasting_binary_for_arithmetics(binary))
    {
        sema_error_at(binary->span, "Can't divide '%s' by '%s'", type_to_string(left->type), type_to_string(right->type));
        return false;
    }
    if (is_both_const(left, right))
    {
        binary->const_expr.value = value_div(left->const_expr.value, right->const_expr.value);
        // TODO handle error
        binary->expr_id = EXPR_CONST;
    }
    return true;
}

static inline bool analyse_mod_expr(Expr *binary)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;
    if (!type_is_int(left->type) || !type_is_int(right->type))
    {
        sema_error_at(binary->span, "Can't take remainder of '%s' from '%s'", type_to_string(left->type), type_to_string(right->type));
        return false;
    }
    if (!try_upcasting_binary_for_arithmetics(binary))
    {
        FATAL_ERROR("Should always succeed");
    }

    if (is_both_const(left, right))
    {
        binary->const_expr.value = value_mod(left->const_expr.value, right->const_expr.value);
        // TODO handle error
        binary->expr_id = EXPR_CONST;
    }
    return true;
}

static inline bool analyse_assign_expr(Expr *binary, Side side)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;

    if (!analyse_expr(left, LHS | side)) return false;
    if (!analyse_expr(right, RHS)) return false;

    // Make sure types match, otherwise try inserting a cast.
    return insert_implicit_cast_if_needed(right, left->type);
}

static inline bool analyse_ne_expr(Expr *binary)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;

    if (!analyse_expr(left, RHS)) return false;
    if (!analyse_expr(right, RHS)) return false;

    TODO
    // Make sure types match, otherwise try inserting a cast.
    return insert_implicit_cast_if_needed(right, left->type);
}

static inline bool analyse_comp_expr(Expr *binary, BinOp op)
{
    Expr *left = binary->binary_expr.left;
    Expr *right = binary->binary_expr.right;

    if (!analyse_expr(left, RHS)) return false;
    if (!analyse_expr(right, RHS)) return false;

    // Make sure types match, otherwise try inserting a cast.
    if (!try_upcasting_binary_for_arithmetics(binary)) return false;
    binary->type = type_builtin_bool();
    return true;
}

bool analyse_binary_expr(Expr *expr, Side side)
{
    LOG_FUNC
    Expr *left = expr->binary_expr.left;
    Expr *right = expr->binary_expr.right;
    switch (expr->binary_expr.operator)
    {
        case BINOP_ASSIGN:
            return analyse_assign_expr(expr, side);
        case BINOP_SUB:
            return analyse_expr(left, RHS)
                   && analyse_expr(right, RHS)
                   && analyse_minus_expr(expr, side);
        case BINOP_ADD:
            return analyse_expr(left, RHS)
                   && analyse_expr(right, RHS)
                   && analyse_plus_expr(expr, side);
        case BINOP_DIV:
            return analyse_expr(left, RHS)
                   && analyse_expr(right, RHS)
                   && analyse_div_expr(expr) && is_rvalue(expr);
        case BINOP_MULT:
            return analyse_expr(left, RHS)
                   && analyse_expr(right, RHS)
                   && analyse_mult_expr(expr) && is_rvalue(expr);
        case BINOP_MOD:
            return analyse_expr(left, RHS)
                   && analyse_expr(right, RHS)
                   &&
                   analyse_mod_expr(expr) && is_rvalue(expr);
        case BINOP_EQ:
        case BINOP_NE:
        case BINOP_GE:
        case BINOP_GT:
        case BINOP_LT:
        case BINOP_LE:
            return analyse_comp_expr(expr, expr->binary_expr.operator);
        case BINOP_MULT_ASSIGN:
        case BINOP_ADD_ASSIGN:
        case BINOP_SUB_ASSIGN:
        case BINOP_DIV_ASSIGN:
        case BINOP_MOD_ASSIGN:
        case BINOP_AND:
        case BINOP_AND_ASSIGN:
        case BINOP_OR:
        case BINOP_OR_ASSIGN:
        case BINOP_BIT_AND:
        case BINOP_BIT_AND_ASSIGN:
        case BINOP_BIT_OR:
        case BINOP_BIT_OR_ASSIGN:
        case BINOP_BIT_XOR:
        case BINOP_BIT_XOR_ASSIGN:
        case BINOP_SHR:
        case BINOP_SHR_ASSIGN:
        case BINOP_SHL:
        case BINOP_SHL_ASSIGN:
        case BINOP_ELVIS:
            FATAL_ERROR("TODO");
        case BINOP_ERROR:
            UNREACHABLE;
    }
    UNREACHABLE;
}


bool analyse_implicit_bool_cast(Expr *expr)
{
    if (type_may_convert_to_bool(expr->type))
    {
        sema_error_at(expr->span, "The expression cannot be implictly converted to boolean");
        return false;
    }
    return true;
}

bool insert_bool_cast_for_conditional_if_needed(Expr *expr)
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
        TypeOld *type = try_upcasting_for_arithmetics(then_expr, else_expr);
        if (!type)
        {
            sema_error_at(expr->span, "Ternary expression result has incompatible types '%s' and '%s'",
                    type_to_string(else_expr->type), type_to_string(then_expr->type));
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



static CastResult perform_compile_time_cast_on_const(Expr *expr, TypeOld *target_type, bool implicit)
{
    if (!cast_const_type_expression(expr, target_type, implicit))
    {
        assert(expr->cast_expr.implicit);
        sema_error_at(expr->span, "Cannot implicitly cast '%s' to '%s'",
                      type_to_string(expr->cast_expr.type), type_to_string(target_type));
        return CAST_FAILED;
    }
    expr->type = target_type;
    return CAST_INLINE;
}

static CastResult perform_compile_time_cast(Expr *expr, TypeOld *target_type, bool implicit)
{
    TypeOld *source_type = expr->type;
    if (source_type == target_type)
    {
        return CAST_INLINE;
    }
    bool is_same_type = source_type->type_id == target_type->type_id;
    target_type = type_unfold_non_opaque(target_type);
    switch (target_type->type_id)
    {
        case XTYPE_INVALID:
        case XTYPE_UNRESOLVED:
        case XTYPE_RESOLVED:
        case XTYPE_ALIAS:
            // Since we resolved the type successfully
            UNREACHABLE
        case XTYPE_VOID:
        case XTYPE_NIL:
        case XTYPE_OPAQUE:
        case XTYPE_TYPEVAL:
        case XTYPE_ARRAY:
        case XTYPE_CONST_INT:
            sema_error_at(expr->span, "Cannot cast expression to '%s'", type_to_string(target_type));
            return CAST_FAILED;
        case XTYPE_CONST_FLOAT:
            if (source_type->type_id == XTYPE_CONST_INT)
            {
                expr->const_expr.value.f = bigint_as_float(&expr->const_expr.value.big_int);
                expr->const_expr.value.float_bits = 0;
                expr->type = target_type;
                expr->const_expr.value.type = VALUE_TYPE_FLOAT;
                return CAST_INLINE;
            }
            break;
        case XTYPE_STRING:
            if (is_same_type)
            {
                // Inline cast
                return CAST_INLINE;
            }
            // Default error
            break;
        case XTYPE_POINTER:
            // Pointer and array types can be immediately converted.
            if (source_type->type_id == XTYPE_POINTER || source_type->type_id == XTYPE_ARRAY) return CAST_PTRPTR;
            // Explicit pointer to int conversions are fine.
            if (type_is_int(target_type) && !implicit) return CAST_INTPTR;
            // Default error
            break;
        case XTYPE_UNION:
        case XTYPE_STRUCT:
        case XTYPE_FUNC_TYPE:
        case XTYPE_FUNC:
            if (is_same_type && source_type->decl == target_type->decl)
            {
                // Inline cast
                expr->type = target_type;
                return CAST_INLINE;
            }
        case XTYPE_ENUM:
            return perform_compile_time_cast(expr, target_type->decl->enum_decl.type, implicit);
        case XTYPE_INT:
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                return perform_compile_time_cast_on_const(expr, target_type, implicit);
            }
            switch (source_type->type_id)
            {
                case XTYPE_NIL:
                case XTYPE_CONST_FLOAT:
                case XTYPE_CONST_INT:
                    UNREACHABLE
                case XTYPE_FLOAT:
                    return target_type->integer.is_signed ? CAST_FPSI : CAST_FPUI;
                case XTYPE_INT:
                    assert(target_type->integer.bits != source_type->integer.bits || target_type->integer.is_signed != source_type->integer.is_signed);
                    if (source_type->integer.is_signed) return target_type->integer.is_signed ? CAST_SISI : CAST_SIUI;
                    return target_type->integer.is_signed ? CAST_UISI : CAST_UIUI;
                case XTYPE_BOOL:
                    return target_type->integer.is_signed ? CAST_UISI : CAST_UIUI;
                case XTYPE_POINTER:
                case XTYPE_ARRAY:
                    // Any pointers can be cast to the non-float builtins.
                    if (!implicit) return CAST_PTRINT;
                    break;
                default:
                    break;
            }
        case XTYPE_FLOAT:
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                return perform_compile_time_cast_on_const(expr, target_type, implicit);
            }
            switch (source_type->type_id)
            {
                case XTYPE_NIL:
                case XTYPE_CONST_FLOAT:
                case XTYPE_CONST_INT:
                    UNREACHABLE
                case XTYPE_FLOAT:
                    return CAST_FPFP;
                case XTYPE_INT:
                    return source_type->integer.is_signed ? CAST_SIFP : CAST_UIFP;
                case XTYPE_BOOL:
                    return CAST_UIFP;
                default:
                    break;
            }
        case XTYPE_BOOL:
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                return perform_compile_time_cast_on_const(expr, target_type, implicit);
            }
            switch (source_type->type_id)
            {
                case XTYPE_NIL:
                case XTYPE_CONST_FLOAT:
                case XTYPE_CONST_INT:
                    UNREACHABLE
                case XTYPE_FLOAT:
                    return CAST_FPUI;
                case XTYPE_INT:
                    return source_type->integer.is_signed ? CAST_SIUI : CAST_UIUI;
                case XTYPE_BOOL:
                    UNREACHABLE
                default:
                    break;
            }
    }
    sema_error_at(expr->span, "Cannot cast '%s' to '%s'", type_to_string(source_type), type_to_string(target_type));
    return CAST_FAILED;
}

static bool analyse_cast_expr(Expr *expr, Side side)
{
    if (!analyse_expr(expr->cast_expr.expr, side)) return false;
    if (!resolve_type(expr->cast_expr.type, false)) return false;
    TypeOld *target_type = expr->cast_expr.type;
    TypeOld *source_type = expr->cast_expr.expr->type;
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
    TypeOld *type = expr->type_expr.type;
    if (!resolve_type(type, false)) return false;
    // Type now resolves, we can set the expression
    TypeOld *type_of_type = new_type(XTYPE_TYPEVAL, false);
    type_of_type->is_public = type->is_public;
    type_of_type->is_exported = type->is_exported;
    type_of_type->module = type->module;
    type_of_type->type_of_type = type;
    expr->type = type_of_type;
    return true;
}

static inline bool force_cast_to_signed(Expr *sub_expr)
{
    TypeOld *type = sub_expr->type;
    switch (type->type_id)
    {
        case XTYPE_CONST_FLOAT:
        case XTYPE_CONST_INT:
        case XTYPE_FLOAT:
            return true;
        case XTYPE_INT:
            if (!type->integer.is_signed)
            {
                TypeOld *target_type = type_get_signed(type);
                return insert_cast_if_needed(sub_expr, target_type, false);
            }
            return true;
        case XTYPE_BOOL:
            insert_cast_if_needed(sub_expr, type_builtin_i8(), false);
            return true;
        default:
            return false;
    }
}
static bool resolve_identifier(Expr *expr, Side side)
{
    LOG_FUNC
    if (expr->identifier_expr.resolved) return true;
    Decl *decl = scope_find_symbol(expr->identifier_expr.identifier, false, false, expr->span);
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
            if (side & LHS)
            {
                sema_error_at(expr->span, "Only variables can be assigned to");
                return false;
            }
            expr->type = decl->type;
            break;
        case DECL_VAR:
            decl->var.in_init = true;
            // TODO if (!resolve_type(decl->type, decl->is_public)) return false;
            if (!resolve_type(decl->var.original_type, decl->is_public)) return false;
            if (!analyse_expr(decl->var.init_expr, RHS)) return false;
            decl->var.in_init = false;
            if (decl->var.in_init)
            {
                sema_error_at(expr->span, "Used '%s' in own initialization", expr->identifier_expr.identifier);
                return false;
            }
            // TODO if ((side & LHS) && type_is_const2(decl->type))
            {
                sema_error_at(expr->span, "Cannot assign to constant value");
                return false;
            }
            // TODO if (type_is_const2(decl->type))
            {
                expr_replace(expr, decl->var.init_expr);
                expr->type = decl->type;
            }
            // TODO else
            {
                expr->type = decl->type;
                expr->identifier_expr.is_ref = side & LHS;
                DEBUG_LOG("%s not resolved as const", expr->identifier_expr.identifier);
            }
            break;
    }
    if (side & RHS)
    {
        decl->is_used = true;
        // TODO
        // if (usedPublicly) decl->is_used_public = true;
    }
    return true;
}

bool analyse_const_expr(Expr *expr)
{
    assert(EXPR_CONST == expr->expr_id);
    assert(expr->type != NULL);
    return true;
}

bool analyse_unary_expr(Expr *expr, Side side)
{
    LOG_FUNC
    Expr *sub_expr = expr->unary_expr.expr;
    switch (expr->unary_expr.operator)
    {
        case UNARYOP_INC:
        case UNARYOP_DEC:
            if (!analyse_expr(sub_expr, LHS | side)) return false;
/*            if (LType.isNull()) return 0;
            checkAssignment(SubExpr, LType);
            expr->setType(LType);*/
            return true;
        case UNARYOP_ADDR:
            if (!analyse_expr(sub_expr, side | RHS)) return false;
            expr->type = type_new_pointer(sub_expr->type);
            // TODO constness copy qualifier if variable.
            return true;
        case UNARYOP_DEREF:
            if (!analyse_expr(sub_expr, side | RHS)) return false;
            if (expr->type->type_id != XTYPE_POINTER)
            {
                sema_error_at(sub_expr->span, "Expected a pointer, not a '%s'", type_to_string(expr->type));
                return false;
            }
            // TODO constness;
            expr->type = expr->type->pointer.base;
            return true;
        case UNARYOP_NEG:
            if (!analyse_expr(sub_expr, side | RHS)) return false;
            if (!force_cast_to_signed(sub_expr)) return false;
            if (sub_expr->expr_id == EXPR_CONST)
            {
                sub_expr->const_expr.value = value_negate(sub_expr->const_expr.value);
                expr_replace(expr, sub_expr);
            }
            expr->type = sub_expr->type;
            return true;
        case UNARYOP_BITNEG:
            if (!analyse_expr(sub_expr, side | RHS)) return false;
            switch (type_unfold_non_opaque(sub_expr->type)->type_id)
            {
                case XTYPE_INT:
                case XTYPE_BOOL:
                    break;
                default:
                    sema_error_at(sub_expr->span,
                                  "Cannot bitwise negate an expression of type '%s'",
                                  type_to_string(sub_expr->type));
                    return false;
            }
            if (sub_expr->expr_id == EXPR_CONST)
            {
                sub_expr->const_expr.value = value_bit_not(sub_expr->const_expr.value);
                assert(sub_expr->const_expr.value.type != VALUE_TYPE_ERROR);
                expr_replace(expr, sub_expr);
                return true;
            }
            expr->type = type_unfold_non_opaque(sub_expr->type);
            return true;
        case UNARYOP_NOT:
            if (!analyse_expr(sub_expr, side | RHS)) return false;
            switch (sub_expr->type->type_id)
            {
                case XTYPE_CONST_INT:
                case XTYPE_FLOAT:
                case XTYPE_POINTER:
                case XTYPE_NIL:
                case XTYPE_INT:
                case XTYPE_BOOL:
                    break;
                default:
                    sema_error_at(sub_expr->span, "Cannot negate '%s'", type_to_string(sub_expr->type));
                    return false;
            }
            if (sub_expr->expr_id == EXPR_CONST)
            {
                expr->const_expr.value = value_not(sub_expr->const_expr.value);
                expr->expr_id = EXPR_CONST;
            }
            expr->type = type_builtin_bool();
            return true;
        case UNARYOP_ERROR:
            return false;
    }
    UNREACHABLE
}

bool check_call_args(Expr *expr, Decl *function, Expr *struct_function)
{

    unsigned func_args = function->func_decl.args->size;
    unsigned call_args = expr->call_expr.parameters->size;

    unsigned func_index = 0;
    unsigned call_index = 0;

    const bool is_struct_function = struct_function != NULL;

    unsigned diagIndex = 0;

    /*
    if (isStructFunction) {
        if (exprIsType(structFunction)) {
            diagIndex = 5;
        }
        else {
            diagIndex = 4;
            funcArgs--;
            funcIndex = 1;
        }
    }*/

    unsigned min_args = MIN(func_args, call_args);

    bool success = true;
    for (unsigned i = 0; i < min_args; i++)
    {
        Expr *call_arg = expr->call_expr.parameters->entries[i];
        if (!analyse_expr(call_arg, RHS))
        {
            success = false;
            continue;
        }
        Decl *func_arg = function->func_decl.args->entries[i];
        TypeOld *type = func_arg->type;
        if (!type_is_same(type, call_arg->type))
        {
            TODO
            success = false;
        }
//        callIndex++;
  //      funcIndex++;
    }
    if (call_args > func_args)
    {
        if (!function->func_decl.variadic)
        {

        }
    }
    /*
    if (callArgs > funcArgs) {
        // more args given, check if function is variadic
        if (!func->isVariadic()) {
            Expr* arg = call->getArg(callIndex);
            unsigned msg = diag::err_typecheck_call_too_many_args;
            if (func->hasDefaultArgs()) msg = diag::err_typecheck_call_too_many_args_at_most;
            Diag(arg->getLocation(), msg) << diagIndex << funcArgs << callArgs;
            return false;
        }
        for (unsigned i = minArgs; i < callArgs; i++) {
            Expr* callArg = call->getArg(callIndex);
            QualType callArgType = analyseExpr(callArg, RHS);
            // TODO use canonical
            if (callArgType == Type::Void()) {
                fprintf(stderr, "ERROR: (TODO) passing 'void' to parameter of incompatible type '...'\n");
                TODO;
                //Diag(callArg->getLocation(), diag::err_typecheck_convert_incompatible)
                //    << "from" << "to" << 1;// << callArg->getLocation();
            }
            callIndex++;
        }
    }
    else if (callArgs < funcArgs) {
        // less args given, check for default argument values
        for (unsigned i = minArgs; i < funcArgs; i++) {
            VarDecl* arg = func->getArg(funcIndex++);
            if (!arg->getInitValue()) {
                unsigned msg = diag::err_typecheck_call_too_few_args;
                if (func->hasDefaultArgs()) {
                    funcArgs = func->minArgs();
                    if (isStructFunction) funcArgs--;
                    msg = diag::err_typecheck_call_too_few_args_at_least;
                }
                Diag(call->getLocEnd(), msg) << diagIndex << funcArgs << callArgs;
                return false;
            }
        }
    }*/
    TODO
    return true;
}
bool analyse_call(Expr *expr, Side side)
{
    if (active_analyser->call_stack_current >= CALL_STACK_DEPTH)
    {
        sema_error_at(expr->span, "Reach max indirection depth");
        return false;
    }

    active_analyser->call_stack[active_analyser->call_stack_current] = NULL;
    active_analyser->call_stack_current++;

    bool success = analyse_expr(expr->call_expr.function, RHS);

    active_analyser->call_stack_current--;

    if (!success) return false;

    Expr *struct_function = active_analyser->call_stack[active_analyser->call_stack_current];

    if (expr->type->type_id != XTYPE_FUNC)
    {
        sema_error_at(expr->span, "'%s' cannot be called as a function", type_to_string(expr->type));
        return false;
    }
    Decl *decl = expr->type->decl;

    decl->is_used = true;
    expr->type = decl->func_decl.rtype;
    expr->call_expr.is_struct_function = struct_function != NULL;
    return check_call_args(expr, decl, struct_function);
}

bool analyse_expr(Expr *expr, Side side)
{
    LOG_FUNC
    expr->is_lvalue = side == LHS;
    switch (expr->expr_id)
    {
        case EXPR_TYPE:
            return analyse_type_expr(expr) && is_rvalue(expr);
        case EXPR_CAST:
            return analyse_cast_expr(expr, side);
        case EXPR_CONST:
            return analyse_const_expr(expr) && is_rvalue(expr);
        case EXPR_BINARY:
            return analyse_binary_expr(expr, side);
        case EXPR_TERNARY:
            return analyse_ternary_expr(expr, side);
        case EXPR_UNARY:
            return analyse_unary_expr(expr, side);
        case EXPR_POST:
            //return analyse_post_expr(expr, side);
        case EXPR_IDENTIFIER:
            return resolve_identifier(expr, side);
        case EXPR_CALL:
            return analyse_call(expr, side);
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
#endif

bool analyse_expr(Expr *expr, Side side) { return  false; }
bool analyse_init_expr(Decl *decl) { return  false; }
bool insert_implicit_cast_if_needed(Expr *expr, TypeOld *type) { return false; }
bool insert_bool_cast_for_conditional_if_needed(Expr *expr)  { return  false; }
bool analyse_implicit_bool_cast(Expr *expr)  { return  false; }

