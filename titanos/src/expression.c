//
// Created by Christoffer Lerno on 2018-07-23.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include "expression.h"
#include "memory.h"

#define MAX_EXPRESSIONS 0xFFFF

#define GROW_EXPRESSION_CAPACITY(c) ((c) < 128 ? 128 : (c) * 2)

typedef struct
{
	Expression *list;
	size_t size;
	size_t capacity;
} ExpressionList;

static ExpressionList all_expressions;

Expression *allocate_expression()
{
	if (all_expressions.size == all_expressions.capacity)
	{
		size_t new_capacity = GROW_EXPRESSION_CAPACITY(all_expressions.capacity);
		all_expressions.list = GROW_ARRAY(all_expressions.list, Expression, all_expressions.capacity, new_capacity);
		all_expressions.capacity = new_capacity;
	}
	Expression *expression = &all_expressions.list[all_expressions.size];
	expression->expression_id = (uint32_t)all_expressions.size++;
	return expression;
}

static inline Expression *by_id(uint32_t expression_id)
{
	return &all_expressions.list[expression_id];
}

Expression *from_id(uint32_t id) { return by_id(id); }

void expr_add_param(Expression *expression, Expression *param)
{
	expression->expression_params[expression->params++] = param->expression_id;
}


bool expression_is_pure(Expression *expression)
{
	// IMPROVE memoization?
	for (int i = 0; i < expression->params; i++)
	{
		// IMPROVE Speedup!
		if (!expression_is_pure(by_id(expression->expression_params[i]))) return false;
	}
	switch (expression->type)
	{
		case NOP:
		case STRING_EXPRESSION:
		case INTEGER_EXPRESSION:
		case DECIMAL_EXPRESSION:
		case IDENT_EXPRESSION:
		case BINARY_EXPRESSION:
		case UNARY_EXPRESSION:
		case OR_EXPRESSION:
		case AND_EXPRESSION:
		case DEREF_EXPRESSION:
		case ADDROF_EXPRESSION:
			return true;
		case CALL_EXPRESSION:
			// ??
			return false;
		case RETURN_EXPRESSION:
		case COPY_EXPRESSION:
		case LOOP_EXPRESSION:
		case ENDLOOP_EXPRESSION:
		case STMT_EXPRESSION:
			return false;
	}
}


Expression *expr_new_of_type(ExpressionType type)
{
	printf("Allocated expression of type %d\n", type);
	Expression *expression = allocate_expression();
	expression->type = type;
    expression->params = 0;
	return expression;
}
