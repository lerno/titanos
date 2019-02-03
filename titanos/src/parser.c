//
// Created by Christoffer Lerno on 2018-07-22.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

#include "parser.h"
#include "parsing.h"
#include "component.h"
#include "module.h"

void init_parser(Parser *parser, const char *filename, bool is_interface)
{
	setup_parse_rules();
	parser->is_interface = is_interface;
	parser->current_module.length = 0;
	parser->imports = new_vector(16);
	parser->types = new_vector(32);
	parser->enum_values = new_vector(32);
	parser->variables = new_vector(8);
	parser->functions = new_vector(8);
	parser->filename = filename;
}

