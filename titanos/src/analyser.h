#pragma once
//
// Created by Christoffer Lernö on 2019-02-01.
//

#include "ast_types.h"
#include "parser.h"
#include "scope.h"

typedef struct _Label
{
    Ast *gotoAst;
    Ast *labelAst;
} Label;

typedef struct _Analyser
{
    Module *module;
    Parser *parser;
    Scope scope;
    Ast *current_func;
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

extern Analyser *analyser;