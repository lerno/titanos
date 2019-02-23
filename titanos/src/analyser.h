#pragma once
//
// Created by Christoffer Lernö on 2019-02-01.
//

#include "ast_types.h"
#include "parser.h"
#include "scope.h"

typedef struct _Analyser
{
    Module *module;
    Parser *parser;
    Scope scope;
    Decl *current_func;
    Vector labels;
    Vector gotos;
    Vector defers;
    /*
     *   AST& ast;
         const Module& module;
         std::unique_ptr<Scope> globals;
         std::unique_ptr<TypeResolver> TR;
         c2lang::DiagnosticsEngine& Diags;
         FunctionAnalyser functionAnalyser;
         bool verbose;
     */
} Analyser;

extern __thread Analyser *active_analyser;
void select_analyser(Analyser *current_analyser);
STable *push_scratch_table();
void pop_scratch_table(STable *table);
Vector *push_scratch_vector();
void pop_scratch_vector(Vector *vector);
