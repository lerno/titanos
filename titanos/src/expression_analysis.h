#pragma once
//
// Created by Christoffer Lernö on 2019-02-14.
//


#include "common.h"
#include "analyser.h"

typedef enum
{
    RHS,
    LHS
} Side;

bool analyse_expr(Expr *expr, Side side);
bool analyse_init_expr(Decl *decl);
bool cast_to_type(Expr *expression, Type *type);
