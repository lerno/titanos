#pragma once

#include "common.h"
#include "parser.h"

Parser *parse(const char *filename, bool is_interface);
void setup_parse_rules(void);
