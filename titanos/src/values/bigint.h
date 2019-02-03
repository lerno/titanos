#pragma once
//
// Created by Christoffer Lernö on 2019-01-27.
// Based on BigInt from ZigLang
//

typedef struct _BigInt
{
    unsigned digit_count;
    bool is_negative;
    union {
        uint64_t digit;
        uint64_t *digits;
    };
} BigInt;

typedef enum _IntCmp
{
    INT_LT,
    INT_GT,
    INT_EQ,
} IntCmp;

void bigint_init_unsigned(BigInt *big_int, uint64_t value);
void bigint_init_signed(BigInt *big_int, int64_t value);
void bigint_init_bigint(BigInt *dest, const BigInt *src);
void bigint_init_data(BigInt *dest, const uint64_t *digits, unsigned int digit_count, bool is_negative);
void bigint_negate(BigInt *dest, const BigInt *source);
size_t bigint_clz(const BigInt *big_int, size_t bit_count);
size_t bigint_ctz(const BigInt *big_int, size_t bit_count);
bool bigint_fits_in_bits(const BigInt *big_int, size_t bit_count, bool is_signed);
void bigint_write_twos_complement(const BigInt *big_int, uint8_t *buf, size_t bit_count, bool is_big_endian);
void bigint_read_twos_complement(BigInt *dest, const uint8_t *buf, size_t bit_count, bool is_big_endian, bool is_signed);
void bigint_add(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_add_wrap(BigInt *dest, const BigInt *op1, const BigInt *op2, size_t bit_count, bool is_signed);
void bigint_sub(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_mul(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_mul_wrap(BigInt *dest, const BigInt *op1, const BigInt *op2, size_t bit_count, bool is_signed);
void bigint_rem(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_mod(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_shl(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_shl_int(BigInt *dest, const BigInt *op1, uint64_t shift);
void bigint_shl_trunc(BigInt *dest, const BigInt *op1, const BigInt *op2, size_t bit_count, bool is_signed);
void bigint_shr(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_div_floor(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_or(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_negate_wrap(BigInt *dest, const BigInt *op, size_t bit_count);
void bigint_not(BigInt *dest, const BigInt *op, size_t bit_count, bool is_signed);
bool bigint_eql(BigInt a, BigInt b);
IntCmp bigint_cmp(const BigInt *op1, const BigInt *op2);
IntCmp bigint_cmp_zero(const BigInt *op);
uint32_t bigint_hash(BigInt x);
void bigint_print(BigInt *bigint, uint64_t base);
uint64_t bigint_as_unsigned(const BigInt *bigint);
int64_t bigint_as_signed(const BigInt *bigint);
long double bigint_as_float(const BigInt *bigint);
void bigint_truncate(BigInt *dst, const BigInt *op, size_t bit_count, bool is_signed);
void bigint_incr(BigInt *x);
size_t bigint_popcount_signed(const BigInt *bi, size_t bit_count);
size_t bigint_popcount_unsigned(const BigInt *big_int);
