#pragma once
//
// Created by Christoffer Lernö on 2019-02-01.
//


#include "common.h"
#include "analyser.h"

bool analyse_stmt(Ast *stmt);
bool analyse_compound_stmt_no_scope(Ast *compound_stmt);
bool analyse_compound_stmt_scoped(Ast *compound_stmt);
bool analyse_global_var(Decl *decl);


