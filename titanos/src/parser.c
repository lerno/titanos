//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "parser.h"
#include "parsing.h"

Parser parser;

void init_parser(void)
{
	setup_parse_rules();
	parser.current_module.length = 0;
	parser.imports_to_resolve = new_vector(16);
	parser.types = new_vector(32);
	parser.variables = new_vector(8);
	parser.functions = new_vector(8);
}

