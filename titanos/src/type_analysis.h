#pragma once
//
// Created by Christoffer Lernö on 2019-02-05.
//

#include "types/type.h"
#include "common.h"

void resolve_types(void);
bool resolve_type(Type *type, bool used_public);
bool analyse_func_decl(Decl *decl, bool is_public);
