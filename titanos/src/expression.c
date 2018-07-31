

#include <math.h>
#include "common.h"
#include "lexer.h"

typedef enum
{
	EXPRESSION_INTEGER,
	EXPRESSION_UINTEGER,
	EXPRESSION_FLOAT,
	EXPRESSION_BOOL,
	EXPRESSION_IDENTIFIER,
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_ERROR,

} ExpressionType;


typedef struct _Expression
{
	union {
		uint64_t ui;
		int64_t si;
		long double f;
		bool b;
		struct _Symbol *variable;
		struct _Token operator;
	} value;
	ExpressionType type;
	bool is_constant;
} Expression;

static inline void constant_not(Expression *expression)
{
	switch (expression->type)
	{
		case EXPRESSION_BOOL:
			expression->value.b = !expression->value.b;
			break;
		case EXPRESSION_UINTEGER:
		case EXPRESSION_INTEGER:
			expression->value.b = expression->value.ui == 0;
			expression->type = EXPRESSION_BOOL;
			break;
		case EXPRESSION_FLOAT:
			expression->value.b = expression->value.f == 0;
			expression->type = EXPRESSION_BOOL;
			break;
		default:
			assert(false && "Illegal type for not-folding");
	}
}


static inline void constant_float_cast(Expression *expr)
{
	switch (expr->type)
	{
		case EXPRESSION_BOOL:
			expr->value.f = expr->value.b ? 1.0 : 0.0;
			break;
		case EXPRESSION_UINTEGER:
			expr->value.f = expr->value.ui;
			break;
		case EXPRESSION_INTEGER:
			expr->value.f = expr->value.si;
			break;
		case EXPRESSION_FLOAT:
			return;
		default:
			assert(false && "Illegal type for float cast");
			return;
	}
	expr->type = EXPRESSION_FLOAT;
}

static inline void constant_integer_cast(Expression *expr)
{
	switch (expr->type)
	{
		case EXPRESSION_BOOL:
			expr->value.si = expr->value.b ? 1 : 0;
			break;
		case EXPRESSION_UINTEGER:
			expr->value.si = (int64_t)expr->value.ui;
			break;
		case EXPRESSION_INTEGER:
			return;
		case EXPRESSION_FLOAT:
			expr->value.si = (int64_t)expr->value.f;
		default:
			assert(false && "Illegal type for float cast");
			return;
	}
	expr->type = EXPRESSION_INTEGER;
}

static inline void constant_uinteger_cast(Expression *expr)
{
	switch (expr->type)
	{
		case EXPRESSION_BOOL:
			expr->value.ui = expr->value.b ? 1 : 0;
			break;
		case EXPRESSION_UINTEGER:
			return;
		case EXPRESSION_INTEGER:
			expr->value.ui = (uint64_t)expr->value.si;
			return;
		case EXPRESSION_FLOAT:
			expr->value.ui = (uint64_t)expr->value.f;
		default:
			assert(false && "Illegal type for float cast");
			return;
	}
	expr->type = EXPRESSION_UINTEGER;
}

static inline void constant_bool_cast(Expression *expression)
{
	switch (expression->type)
	{
		case EXPRESSION_BOOL:
			break;
		case EXPRESSION_UINTEGER:
		case EXPRESSION_INTEGER:
			expression->value.b = expression->value.ui != 0;
			expression->type = EXPRESSION_BOOL;
			break;
		case EXPRESSION_FLOAT:
			expression->value.b = expression->value.f != 0;
			expression->type = EXPRESSION_BOOL;
			break;
		default:
			assert(false && "Illegal type for not-folding");
	}
}

#define EXACT_POW_MAX 100

#define INLINE_MULT_POW(type, value, pow) \
do { \
  if (pow == 0) { value = (type)1;} else { \
    type __original = value; \
    for (int i = 0; i < pow; i++) { value *= __original; } \
  } \
} while (0)

static inline bool constant_pow(Expression *left, Expression *right)
{
	if (right->type == EXPRESSION_BOOL || left->type == EXPRESSION_BOOL) return false;

	int integer_power = -1;
	switch (right->type)
	{
		case EXPRESSION_INTEGER:
			integer_power = right->value.si >= 0 && right->value.si <= EXACT_POW_MAX ? (int)right->value.si : -1;
			break;
		case EXPRESSION_UINTEGER:
			integer_power = right->value.ui <= EXACT_POW_MAX ? (int)right->value.ui : -1;
			break;
		case EXPRESSION_FLOAT:
			integer_power = right->value.f == 0 ? 0 : -1;
			break;
		default:
			break;
	}
	if (integer_power > -1)
	{
		switch (left->type)
		{
			case EXPRESSION_INTEGER:
				INLINE_MULT_POW(int64_t, left->value.si, integer_power);
				return true;
			case EXPRESSION_UINTEGER:
				INLINE_MULT_POW(uint64_t, left->value.ui, integer_power);
				return true;
			case EXPRESSION_FLOAT:
				INLINE_MULT_POW(long double, left->value.f, integer_power);
				return true;
			default:
				assert(false && "Should not happen");
		}
	}
	constant_float_cast(left);
	constant_float_cast(right);

	left->value.f = powl(left->value.f, right->value.f);
	return true;
}

static inline void constant_fold_mult(Expression *left, Expression *right)
{
	if (left->type == EXPRESSION_FLOAT || right->type == EXPRESSION_FLOAT)
	{
		constant_float_cast(left);
		constant_float_cast(right);
	}
	else if (left->type == EXPRESSION_INTEGER || right->type == EXPRESSION_INTEGER)
	{
		constant_integer_cast(left);
		constant_integer_cast(right);
	}
	else
	{
		constant_uinteger_cast(left);
		constant_uinteger_cast(right);
	}

	switch (left->type)
	{
		case EXPRESSION_BOOL:
			assert(false && "Can't happen due to casts");
			return;
		case EXPRESSION_UINTEGER:
			left->value.ui *= right->value.ui;
			break;
		case EXPRESSION_INTEGER:
			left->value.si *= right->value.si;
			break;
		case EXPRESSION_FLOAT:
			left->value.f *= right->value.f;
			break;
		default:
			assert(false && "Illegal type for mult-folding");
			break;
	}
}


static inline void constant_fold_plus(Expression *left, Expression *right)
{
	if (left->type == EXPRESSION_FLOAT || right->type == EXPRESSION_FLOAT)
	{
		constant_float_cast(left);
		constant_float_cast(right);
	}
	else if (left->type == EXPRESSION_INTEGER || right->type == EXPRESSION_INTEGER)
	{
		constant_integer_cast(left);
		constant_integer_cast(right);
	}
	else
	{
		constant_uinteger_cast(left);
		constant_uinteger_cast(right);
	}

	switch (left->type)
	{
		case EXPRESSION_BOOL:
			assert(false && "Can't happen due to casts");
			return;
		case EXPRESSION_UINTEGER:
			left->value.ui += right->value.ui;
			break;
		case EXPRESSION_INTEGER:
			left->value.si += right->value.si;
			break;
		case EXPRESSION_FLOAT:
			left->value.f += right->value.f;
			break;
		default:
			assert(false && "Illegal type for mult-folding");
			break;
	}
}

static inline void constant_minus(Expression *expression)
{
	switch (expression->type)
	{
		case EXPRESSION_BOOL:
			expression->value.si = expression->value.b ? -1 : 0;
			expression->type = EXPRESSION_INTEGER;
			break;
		case EXPRESSION_UINTEGER:
			expression->value.si = -(int64_t)expression->value.ui;
			expression->type = EXPRESSION_INTEGER;
			break;
		case EXPRESSION_INTEGER:
			expression->value.si = -expression->value.si;
			break;
		case EXPRESSION_FLOAT:
			expression->value.f = -expression->value.f;
			break;
		default:
			assert(false && "Illegal type for minus-folding");
	}
}

static inline bool constant_bit_not(Expression *expression)
{
	switch (expression->type)
	{
		case EXPRESSION_UINTEGER:
			expression->value.ui = ~expression->value.ui;
			return true;
		case EXPRESSION_INTEGER:
			expression->value.si = ~expression->value.si;
			return true;
		case EXPRESSION_FLOAT:
		case EXPRESSION_BOOL:
			return false;
		default:
			assert(false && "Illegal type for not-folding");
			return false;
	}
}

