#pragma once
//
// Created by Christoffer Lerno on 2018-07-23.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "common.h"
#include "lexer.h"

typedef enum
{
	// Atoms
	NOP,
	STRING_EXPRESSION,
	DECIMAL_EXPRESSION,
	INTEGER_EXPRESSION,
	IDENT_EXPRESSION,
	// Transform
	BINARY_EXPRESSION,
	UNARY_EXPRESSION,
	// Logic
	OR_EXPRESSION,
	AND_EXPRESSION,
	// Pointer
	DEREF_EXPRESSION,
	ADDROF_EXPRESSION,
	// Call
	CALL_EXPRESSION,
	RETURN_EXPRESSION,
	COPY_EXPRESSION,
	// Logic
	LOOP_EXPRESSION,
	ENDLOOP_EXPRESSION,
	STMT_EXPRESSION
} ExpressionType;

#define MAX_EXPRESSION_PARAMS 64

typedef struct _Expression
{
	ExpressionType type;
	uint32_t expression_params[MAX_EXPRESSION_PARAMS];
	uint16_t params;
	union
	{
		int64_t numvalue;
		token_type token_type;
	} data;
	uint32_t expression_id;
} Expression;

bool expression_is_pure(Expression *expression);
void expr_add_param(Expression *expression, Expression *param);

Expression *from_id(uint32_t id);
Expression *expr_new_of_type(ExpressionType type);
Expression *allocate_expression();
