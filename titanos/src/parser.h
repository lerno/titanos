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
    const char *current_module;
    SourceRange module_def;
    Vector *imports; // Decl *
	Vector *types; // Decl *
	Vector *variables;
	Vector *functions;
	Vector *enum_values;
	Vector *array_values; // Remember to add the module to these!
	const char *filename;
} Parser;



void init_parser(Parser *parser, const char *filename, bool is_interface);
