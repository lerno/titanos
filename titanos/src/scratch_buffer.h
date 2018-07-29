#pragma once
//
// Created by Christoffer Lerno on 2018-07-29.
// Copyright (c) 2018 Christoffer Lerno. All rights reserved.
//

void scratch_buffer_clear(void);

char *scratch_buffer_start(void);
int scratch_buffer_length(void);

void write_to_scratch_length(const char *string, int length);
void write_to_scratch(const char *string);
