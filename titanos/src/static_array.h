#pragma once
//
// Created by Christoffer Lernö on 2019-03-07.
//


#define STATIC_ARRAY(__type, __size) struct { __type entry[__size]; uint32_t size; }
#define STATIC_ARRAY_ADD(__array, __value) do { assert(__array.size < (sizeof(__array.entry) / sizeof(__array.entry[0]))); __array.entry[__array.size++] = (__value); } while(0)
#define STATIC_ARRAY_CLEAR(__array) __array.size = 0
