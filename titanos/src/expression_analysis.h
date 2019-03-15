#pragma once
//
// Created by Christoffer Lernö on 2019-02-14.
//


#include "common.h"
#include "analyser.h"

typedef enum
{
    RHS = 0x01,
    LHS = 0x02
} Side;

bool analyse_expr(Expr *expr, Side side);
bool analyse_init_expr(Decl *decl);
bool insert_implicit_cast_if_needed(Expr *expr, TypeOld *type);
bool insert_bool_cast_for_conditional_if_needed(Expr *expr);
bool analyse_implicit_bool_cast(Expr *expr);
bool perform_arithmetic_conversions(Expr *LHS, Expr *RHS, bool is_compile_assign, bool allow_sign_conversion);
bool perform_assignment_conversions(QualifiedType left_type, Expr *RHS);
