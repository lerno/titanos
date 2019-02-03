#pragma once
//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "common.h"
#include "vector.h"
#include "lexer.h"

typedef struct _Parser
{
	bool is_interface : 1;
	unsigned remainder : 31;
    Token current_module;
    Vector *imports;
	Vector *types;
	Vector *variables;
	Vector *functions;
	Vector *enum_values;
	const char *filename;
} Parser;



void init_parser(Parser *parser, const char *filename, bool is_interface);
