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
    Token current_module;
    Vector *imports_to_resolve;
	Vector *types;
	Vector *variables;
	Vector *functions;
} Parser;


extern Parser parser;

void init_parser(void);
