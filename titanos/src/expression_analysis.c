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

static CastResult perform_compile_time_cast(Expr *expr, Type *target_type, bool implicit);

static bool is_rvalue(Expr *expr)
{
    if (expr->is_lvalue)
    {
        sema_error_at(expr->span, "Expression is not assignable");
        return false;
    }
    return true;
}
static inline bool is_castable(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
            return false;
        case TYPE_UNRESOLVED:
            UNREACHABLE;
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_VOID:
        case TYPE_NIL:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_TYPEVAL:
        case TYPE_FLOAT:
        case TYPE_INT:
        case TYPE_BOOL:
        case TYPE_ENUM:
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_RESOLVED:
        case TYPE_ALIAS:
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
    if (type_is_same(expr->type, type)) return true;
    Type *target_type = type_unfold_non_opaque(type);
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
    expr->type = target_type;
    expr->cast_expr.type = target_type;
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
    if (!insert_implicit_cast_if_needed(decl->var.init_expr, decl->type))
    {
        sema_error_at(decl->span, "Cannot implictly cast expression to '%s'", type_to_string(decl->type));
        return false;
    }
    return true;
}


typedef Value (*BinOpFunc)(Value, Value);

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

    switch (non_pointer_type->type_id)
    {
        case TYPE_INT:
            return pointer_expr->type;
        case TYPE_BOOL:
            return insert_implicit_cast_if_needed(non_pointer_expr, type_builtin_u8()) ? pointer_expr->type : NULL;
        default:
            return NULL;
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
    switch (type_unfold_non_opaque(type)->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_TYPEVAL:
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_RESOLVED:
        case TYPE_ALIAS:
            return false;
        case TYPE_ENUM:
        case TYPE_NIL:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_BOOL:
            return true;
    }
}

bool is_real_arithmetics_type(Type *type)
{
    switch (type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_TYPEVAL:
        case TYPE_BOOL:
        case TYPE_NIL:
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_RESOLVED:
        case TYPE_ALIAS:
            return false;
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_ENUM:
            return true;
    }
}

bool default_function_array_conversion(Expr *expr)
{
    // Or func type??
    if (expr->type->type_id == TYPE_FUNC)
    {
        return insert_implicit_cast_if_needed(expr, type_new_pointer(expr->type));
    }
    if (expr->type->type_id == TYPE_ARRAY)
    {
        return insert_implicit_cast_if_needed(expr, type_new_pointer(expr->type->array.base));
    }
    return expr;
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

    if (expr->type->type_id == TYPE_VOID) return true;

    if (expr->type->qualifier)
    {
        expr->type = type_remove_qualifier(expr->type);
    }

    // add lvalue->rvalue cast

    //646   ExprResult Res = ImplicitCastExpr::Create(Context, T, CK_LValueToRValue, E,

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
    Type *type = expr->type;
    switch (type->type_id)
    {
        case TYPE_INT:
            // TODO handle bitfield
            break;
        case TYPE_ENUM:
            expr->type = type->decl->enum_decl.type;
            break;
        default:
            // Do nothing.
            break;
    }

    return true;
}
static bool perform_arithmetic_conversions(Expr *LHS, Expr *RHS, bool is_compile_assign)
{
    if (!is_compile_assign && !perform_unary_conversions(LHS)) return false;
    if (!perform_unary_conversions(RHS)) return false;
    Type *left = type_remove_qualifier(type_canonical(LHS->type));
    Type *right = type_remove_qualifier(type_canonical(RHS->type));

    if (type_is_same(left, right)) return true; // or left == right
    if (!is_arithmetics_type(left) || !is_arithmetics_type(right)) return false;
/*
    if (type_is_int_promotable(left))
    {
        left = type_promoted_int(left);
    }
    if (type_is_promotable_bitfield(left))
    {
        left = type_promotable
    }
    if /
    QualType LHSUnpromotedType = LHSType;
    1377   if (LHSType->isPromotableIntegerType())
        1378     LHSType = Context.getPromotedIntegerType(LHSType);
    1379   QualType LHSBitfieldPromoteTy = Context.isPromotableBitField(LHS.get());
    1380   if (!LHSBitfieldPromoteTy.isNull())
        1381     LHSType = LHSBitfieldPromoteTy;
    1382   if (LHSType != LHSUnpromotedType && !IsCompAssign)
        1383     LHS = ImpCastExprToType(LHS.get(), LHSType, CK_IntegralCast);
    /*
1375   // Apply unary and bitfield promotions to the LHS's type.
1376   QualType LHSUnpromotedType = LHSType;
1377   if (LHSType->isPromotableIntegerType())
1378     LHSType = Context.getPromotedIntegerType(LHSType);
1379   QualType LHSBitfieldPromoteTy = Context.isPromotableBitField(LHS.get());
1380   if (!LHSBitfieldPromoteTy.isNull())
1381     LHSType = LHSBitfieldPromoteTy;
1382   if (LHSType != LHSUnpromotedType && !IsCompAssign)
1383     LHS = ImpCastExprToType(LHS.get(), LHSType, CK_IntegralCast);
1384
1385   // If both types are identical, no conversion is needed.
1386   if (LHSType == RHSType)
1387     return LHSType;
1388
1389   // At this point, we have two different arithmetic types.
1390
1391   // Diagnose attempts to convert between __float128 and long double where
1392   // such conversions currently can't be handled.
1393   if (unsupportedTypeConversion(*this, LHSType, RHSType))
1394     return QualType();
1395
1396   // Handle complex types first (C99 6.3.1.8p1).
1397   if (LHSType->isComplexType() || RHSType->isComplexType())
1398     return handleComplexFloatConversion(*this, LHS, RHS, LHSType, RHSType,
1399                                         IsCompAssign);
1400
1401   // Now handle "real" floating types (i.e. float, double, long double).
1402   if (LHSType->isRealFloatingType() || RHSType->isRealFloatingType())
1403     return handleFloatConversion(*this, LHS, RHS, LHSType, RHSType,
1404                                  IsCompAssign);
1405
1406   // Handle GCC complex int extension.
1407   if (LHSType->isComplexIntegerType() || RHSType->isComplexIntegerType())
1408     return handleComplexIntConversion(*this, LHS, RHS, LHSType, RHSType,
1409                                       IsCompAssign);
1410
1411   if (LHSType->isFixedPointType() || RHSType->isFixedPointType())
1412     return handleFixedPointConversion(*this, LHSType, RHSType);
1413
1414   // Finally, we have two differing integer types.
1415   return handleIntegerConversion<doIntegralCast, doIntegralCast>
1416            (*this, LHS, RHS, LHSType, RHSType, IsCompAssign);
1417 }
1418*/
    return false;
}

static inline Type *try_upcasting_for_arithmetics(Expr *left, Expr *right)
{
    Type *left_type = type_unfold_non_opaque(left->type);
    Type *right_type = type_unfold_non_opaque(right->type);

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
    Type *left_type = type_unfold_non_opaque(left->type);
    Type *right_type = type_unfold_non_opaque(right->type);
    if (left_type->type_id == TYPE_POINTER)
    {
        // TODO pointer int type
        Type *target = type_is_signed(right_type) ? type_builtin_i64() : type_builtin_u64();
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
        Type *type = try_upcasting_for_arithmetics(then_expr, else_expr);
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
        case TYPE_RESOLVED:
        case TYPE_ALIAS:
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_OPAQUE:
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_TYPEVAL:
        case TYPE_FUNC:
        case TYPE_FUNC_TYPE:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_NIL:
            UNREACHABLE
        case TYPE_INT:
            return value_convert(value, VALUE_TYPE_INT, target_type->integer.bits, !target_type->integer.is_signed, allow_trunc);
        case TYPE_FLOAT:
            return value_convert(value, VALUE_TYPE_FLOAT, target_type->float_bits, false, allow_trunc);
        case TYPE_BOOL:
            return value_convert(value, VALUE_TYPE_BOOL, 1, true, allow_trunc);
        case TYPE_CONST_FLOAT:
            return value_convert(value, VALUE_TYPE_FLOAT, 0, false, allow_trunc);
        case TYPE_CONST_INT:
            return value_convert(value, VALUE_TYPE_INT, 0, false, allow_trunc);
        case TYPE_ENUM:
            return cast_const_type_expression(expr, target_type->decl->enum_decl.type, is_implicit);
    }
    UNREACHABLE
}

static CastResult perform_compile_time_cast_on_const(Expr *expr, Type *target_type, bool implicit)
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

static CastResult perform_compile_time_cast(Expr *expr, Type *target_type, bool implicit)
{
    Type *source_type = expr->type;
    if (source_type == target_type)
    {
        return CAST_INLINE;
    }
    bool is_same_type = source_type->type_id == target_type->type_id;
    target_type = type_unfold_non_opaque(target_type);
    switch (target_type->type_id)
    {
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_RESOLVED:
        case TYPE_ALIAS:
            // Since we resolved the type successfully
            UNREACHABLE
        case TYPE_VOID:
        case TYPE_NIL:
        case TYPE_OPAQUE:
        case TYPE_TYPEVAL:
        case TYPE_ARRAY:
        case TYPE_CONST_INT:
            sema_error_at(expr->span, "Cannot cast expression to '%s'", type_to_string(target_type));
            return CAST_FAILED;
        case TYPE_CONST_FLOAT:
            if (source_type->type_id == TYPE_CONST_INT)
            {
                expr->const_expr.value.f = bigint_as_float(&expr->const_expr.value.big_int);
                expr->const_expr.value.float_bits = 0;
                expr->type = target_type;
                expr->const_expr.value.type = VALUE_TYPE_FLOAT;
                return CAST_INLINE;
            }
            break;
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
        case TYPE_UNION:
        case TYPE_STRUCT:
        case TYPE_FUNC_TYPE:
        case TYPE_FUNC:
            if (is_same_type && source_type->decl == target_type->decl)
            {
                // Inline cast
                expr->type = target_type;
                return CAST_INLINE;
            }
        case TYPE_ENUM:
            return perform_compile_time_cast(expr, target_type->decl->enum_decl.type, implicit);
        case TYPE_INT:
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                return perform_compile_time_cast_on_const(expr, target_type, implicit);
            }
            switch (source_type->type_id)
            {
                case TYPE_NIL:
                case TYPE_CONST_FLOAT:
                case TYPE_CONST_INT:
                    UNREACHABLE
                case TYPE_FLOAT:
                    return target_type->integer.is_signed ? CAST_FPSI : CAST_FPUI;
                case TYPE_INT:
                    assert(target_type->integer.bits != source_type->integer.bits || target_type->integer.is_signed != source_type->integer.is_signed);
                    if (source_type->integer.is_signed) return target_type->integer.is_signed ? CAST_SISI : CAST_SIUI;
                    return target_type->integer.is_signed ? CAST_UISI : CAST_UIUI;
                case TYPE_BOOL:
                    return target_type->integer.is_signed ? CAST_UISI : CAST_UIUI;
                case TYPE_POINTER:
                case TYPE_ARRAY:
                    // Any pointers can be cast to the non-float builtins.
                    if (!implicit) return CAST_PTRINT;
                    break;
                default:
                    break;
            }
        case TYPE_FLOAT:
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                return perform_compile_time_cast_on_const(expr, target_type, implicit);
            }
            switch (source_type->type_id)
            {
                case TYPE_NIL:
                case TYPE_CONST_FLOAT:
                case TYPE_CONST_INT:
                    UNREACHABLE
                case TYPE_FLOAT:
                    return CAST_FPFP;
                case TYPE_INT:
                    return source_type->integer.is_signed ? CAST_SIFP : CAST_UIFP;
                case TYPE_BOOL:
                    return CAST_UIFP;
                default:
                    break;
            }
        case TYPE_BOOL:
            // Make casts on const expression compile time resolved:
            if (expr->expr_id == EXPR_CONST)
            {
                return perform_compile_time_cast_on_const(expr, target_type, implicit);
            }
            switch (source_type->type_id)
            {
                case TYPE_NIL:
                case TYPE_CONST_FLOAT:
                case TYPE_CONST_INT:
                    UNREACHABLE
                case TYPE_FLOAT:
                    return CAST_FPUI;
                case TYPE_INT:
                    return source_type->integer.is_signed ? CAST_SIUI : CAST_UIUI;
                case TYPE_BOOL:
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
    if (!resolve_type(type, false)) return false;
    // Type now resolves, we can set the expression
    Type *type_of_type = new_type(TYPE_TYPEVAL, false);
    type_of_type->is_public = type->is_public;
    type_of_type->is_exported = type->is_exported;
    type_of_type->module = type->module;
    type_of_type->type_of_type = type;
    expr->type = type_of_type;
    return true;
}

static inline bool force_cast_to_signed(Expr *sub_expr)
{
    Type *type = sub_expr->type;
    switch (type->type_id)
    {
        case TYPE_CONST_FLOAT:
        case TYPE_CONST_INT:
        case TYPE_FLOAT:
            return true;
        case TYPE_INT:
            if (!type->integer.is_signed)
            {
                Type *target_type = type_get_signed(type);
                return insert_cast_if_needed(sub_expr, target_type, false);
            }
            return true;
        case TYPE_BOOL:
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
            if (!resolve_type(decl->type, decl->is_public)) return false;
            if (!resolve_type(decl->var.original_type, decl->is_public)) return false;
            if (!analyse_expr(decl->var.init_expr, RHS)) return false;
            decl->var.in_init = false;
            if (decl->var.in_init)
            {
                sema_error_at(expr->span, "Used '%s' in own initialization", expr->identifier_expr.identifier);
                return false;
            }
            if ((side & LHS) && type_is_const(decl->type))
            {
                sema_error_at(expr->span, "Cannot assign to constant value");
                return false;
            }
            if (type_is_const(decl->type))
            {
                expr_replace(expr, decl->var.init_expr);
                expr->type = decl->type;
            }
            else
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
            if (expr->type->type_id != TYPE_POINTER)
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
                case TYPE_INT:
                case TYPE_BOOL:
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
                case TYPE_CONST_INT:
                case TYPE_FLOAT:
                case TYPE_POINTER:
                case TYPE_NIL:
                case TYPE_INT:
                case TYPE_BOOL:
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
        Type *type = func_arg->type;
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

    if (expr->type->type_id != TYPE_FUNC)
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
