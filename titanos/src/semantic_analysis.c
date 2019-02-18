#include <string.h>
#include "types/type.h"
#include "diagnostics.h"
#include "semantic_analysis.h"
#include "vector.h"
#include "ast_types.h"
#include "parser.h"
#include "error.h"
#include "table.h"
#include "arena_allocator.h"
#include "scope.h"
#include "attributes.h"
#include "constant_folding.h"
#include "analyser.h"
#include "statement_analysis.h"
#include "decl.h"
#include "type_analysis.h"

const static uint64_t MAX_ARRAY_SIZE = UINT32_MAX;





static inline void add_imports()
{
    LOG_FUNC;
    for (unsigned i = 0; i < active_analyser->parser->imports->size; i++)
    {
        scope_add_import_declaration(active_analyser->parser->imports->entries[i]);
    }
}

unsigned check_attributes(Ast *ast_attribute_list)
{
    // TODO transfer this
    if (!ast_attribute_list) return 0;
    unsigned errors = 0;
    assert(false);
    Vector *attributes = NULL;
    for (unsigned a = 0; a < attributes->size; a++)
    {
        // Improve: check that same attribute does not appear more than once?
        Ast *attribute = attributes->entries[a];
        assert(attribute->ast_id == AST_ATTRIBUTE);
        Token *name = &attribute->attribute.name;
        AttributeType attribute_type = attribute_type_from_token(name);
        if (attribute_type == ATTRIBUTE_UNKNOWN)
        {
            sema_error_at(&attribute->attribute.name, "Unknown attribute '%.*s'", SPLAT_TOK(*name));
            errors++;
            continue;
        }
        const AttributeInfo *info = attribute_info_from_type(attribute_type);
        Expr *value = attribute->attribute.value;
        if (value)
        {
            if (evaluate_constant(attribute->attribute.value) == CONST_FULL)
            {
                sema_error_at(&attribute->attribute.value->span, "Value must be a constant");
                errors++;
                continue;
            }
        }
        else
        {
            if (info->argument != ATTR_ARG_NONE)
            {
                sema_error_at(&attribute->attribute.name, "Attribute '%s' requires an argument", info->name);
                errors++;
            }
            continue;
        }

        switch (info->argument)
        {
            case ATTR_ARG_NONE:
                sema_error_at(&attribute->span, "Attribute '%s' cannot have arguments", info->name);
                errors++;
                break;
            case ATTR_ARG_STRING:
//                if (value->ast_id != AST_CONST_EXPR || value->const_expr.value.type != VALUE_TYPE_STRING)
                {
                    sema_error_at(&value->span, "Expected a string argument");
                    errors++;
                }
                break;
            case ATTR_ARG_UINT:
  //              if (value->ast_id != AST_CONST_EXPR
    //                || value->const_expr.value.type != VALUE_TYPE_INT
      //              || value->const_expr.value.big_int.is_negative)
                {
                    sema_error_at(&value->span, "Expected a unsigned int value");
                    errors++;
                }
                break;
            case ATTR_ARG_INT:
        //        if (value->ast_id != AST_CONST_EXPR || value->const_expr.value.type != VALUE_TYPE_INT)
                {
                    sema_error_at(&value->span, "Expected an integer value");
                    errors++;
                }
                break;
            case ATTR_ARG_NUMBER:
          //      if (value->ast_id != AST_CONST_EXPR || !value_is_number(&value->const_expr.value))
                {
                    sema_error_at(&value->span, "Expected a numeric value");
                    errors++;
                }
                break;
            case ATTR_ARG_BOOL:
            //    if (value->ast_id != AST_CONST_EXPR || value->const_expr.value.type != VALUE_TYPE_INT)
                {
                    sema_error_at(&value->span, "Expected a boolean value");
                    errors++;
                }
                break;
            default:
                FATAL_ERROR("Unexpected value");
        }
    }
    return errors;
}



static inline bool analyse_body(Decl *func)
{
    bool skip_unused_params_check = decl_has_attribute(func, ATTRIBUTE_UNUSED_PARAMS);

    //assert(func->is_resolved);

    if (active_analyser->parser->is_interface) skip_unused_params_check = true;

    FuncDecl *func_decl = &func->func_decl;

    // add arguments to new scope
    for (unsigned i = 0; i < func_decl->args->size; i++)
    {
        Decl *param_decl = func_decl->args->entries[i];

        assert(param_decl->type.type_id != TYPE_UNRESOLVED && param_decl->type.type_id != TYPE_INVALID);
        if (skip_unused_params_check)
        {
            param_decl->is_used = true;
        }
        if (param_decl->name.length)
        {
            Decl *old = scope_check_scoped_symbol(&param_decl->name);
            if (old)
            {
                sema_error_at(&param_decl->name, "Parameter %.*s shadows global identifier", SPLAT_TOK(param_decl->name));
                prev_at(&old->span, "Previous declaration was here");
                continue;
            }
            scope_add_scoped_symbol(param_decl);
        }
    }

    if (scope_had_errors()) return false;

    if (func->func_decl.body)
    {
        analyse_compound_stmt(func->func_decl.body);
    }
    if (scope_had_errors()) return false;

    // check for return statement of return value is required
    bool needs_return = func_decl->rtype->type_id == TYPE_VOID;

    if (needs_return && func_decl->body)
    {
        Ast *last_stmt = ast_compound_stmt_last(func_decl->body);
        if (!last_stmt || last_stmt->ast_id != AST_RETURN_STMT)
        {
            sema_error_at(last_stmt ? &last_stmt->span : &func_decl->body->span, "Expected a return at the end of a non void function");
            return false;
        }
    }
    return true;
}

static inline unsigned defer_depth(Ast *stmt)
{
    unsigned depth = 0;
    while (stmt)
    {
        stmt = stmt->defer_stmt.prev_defer;
        depth++;
    }
    return depth;
}

static bool analyse_defer_goto()
{
    for (unsigned i = 0; i < active_analyser->gotos.size; i++)
    {
        Ast *goto_stmt = active_analyser->gotos.entries[i];
        Ast *label_defer_top = goto_stmt->goto_stmt.label_stmt->label_stmt.defer_top;;
        Ast *goto_defer_top = goto_stmt->goto_stmt.defer_list.defer_start;

        // First we need to search for the common depth.
        unsigned label_depth = defer_depth(label_defer_top);
        unsigned goto_depth = defer_depth(goto_defer_top);

        Ast *common_depth_label = label_defer_top;
        Ast *common_depth_goto = goto_defer_top;
        if (label_depth > goto_depth)
        {
            for (unsigned j = 0; j < label_depth - goto_depth; j++)
            {
                common_depth_label = common_depth_label->defer_stmt.prev_defer;
            }
        }
        else
        {
            for (unsigned j = 0; j < goto_depth - label_depth; j++)
            {
                common_depth_goto = common_depth_goto->defer_stmt.prev_defer;
            }
        }

        // We will jump up to this level, so we're fine.
        goto_stmt->goto_stmt.defer_list.defer_end = common_depth_goto;

        // The simple case is jumping back, this is just
        // like doing a break.
        // label1:
        // defer do_a();
        // {
        //    defer do_b();
        // }
        // {
        //    defer do_c();
        //    goto label1; // defer list = do_c() -> do_a() -> null
        // }
#ifdef TODOX
Make sure this is set!
        if (gotoStmt->isGotoBack())
        {
            continue;
        }
#endif
        // In the second case we might pass over some defers.
        // goto label1: // defer list = null
        // defer do_a();
        // {
        //    defer do_b();
        // }
        // {
        //    defer do_c();
        //    label1: // defer list = do_c() -> do_a() -> null
        //    print_something()
        // }

        // We do a sweep and mark defers as conditional.
        while (label_defer_top != common_depth_goto)
        {
            label_defer_top->defer_stmt.emit_boolean = true;
            label_defer_top = label_defer_top->defer_stmt.prev_defer;
        }
    }
    return true;
}

static bool analyse_func_body(Decl *func)
{
    LOG_FUNC
    assert(func->type_id == DECL_FUNC);
    bool success = true;
    for (unsigned i = 0; i < func->func_decl.args->size; i++)
    {
        Decl *param_decl = func->func_decl.args->entries[i];
        assert(param_decl->type_id == DECL_VAR && param_decl->var.kind == VARDECL_PARAM);
        if (param_decl->var.type->type_id == TYPE_INVALID) continue;
        if (param_decl->var.init_expr)
        {
            if (evaluate_constant(param_decl->var.init_expr) != CONST_FULL)
            {
                sema_error_at(&param_decl->var.init_expr->span,
                              "Default value must be a compile time constant");
                success = false;
            }

        }
    }
    // Clear labels
    active_analyser->labels.size = 0;
    active_analyser->gotos.size = 0;
    active_analyser->defers.size = 0;
    active_analyser->current_func = func;
    scope_enter(SCOPE_FUNC | SCOPE_DECL);
    success = analyse_body(func) && success;
    scope_exit(func->func_decl.body);

    for (unsigned i = 0; i < active_analyser->labels.size; i++)
    {
        Label *label = active_analyser->labels.entries[i];
        if (label->labelAst)
        {
            if (!label->gotoAst)
            {
                sema_warn_at(DIAG_UNUSED_LABEL, &label->labelAst->span, "Unused label '%.*s'",
                             SPLAT_TOK(label->labelAst->label_stmt.label_name));
            }
        }
        else
        {
            FATAL_ERROR("TODO");
/*            //assert(label->gotoAst->goto_stmt.label->ast_id == AST_CONST_EXPR);
            //assert(label->gotoAst->goto_stmt.label->const_expr.value.type == VALUE_TYPE_STRING);
            Value *goto_label = &label->gotoAst->goto_stmt.label->const_expr.value;
            sema_error_at(&label->gotoAst->span, "Unknown label '%.*s", goto_label->str_len, goto_label->str);*/
            success = false;
        }
    }

    return analyse_defer_goto() && success;
}

bool check_functions()
{
    bool success = true;
    for (unsigned i = 0; i < active_analyser->parser->functions->size; i++)
    {
        Decl *type = active_analyser->parser->functions->entries[i];
        success &= analyse_func_decl(type, type->is_public);
    }
    for (unsigned i = 0; i < active_analyser->parser->functions->size; i++)
    {
        Decl *type = active_analyser->parser->functions->entries[i];
        success &= analyse_func_body(type);
    }
    return success;
}


static inline void add_symbols()
{
    // First, register imports
    for (unsigned i = 0; i < active_analyser->parser->imports->size; i++)
    {
        Decl *import = active_analyser->parser->imports->entries[i];
        assert(import->type_id == DECL_IMPORT);
        Token *name;
        switch (import->import.type)
        {
            case IMPORT_TYPE_LOCAL:
            case IMPORT_TYPE_FULL:
                name = &import->name;
                break;
            case IMPORT_TYPE_ALIAS:
                name = &import->import.alias;
                break;
            default:
                FATAL_ERROR("Unknown type");
        }
        Decl *old = module_add_symbol(active_analyser->module, name, import);
        if (old)
        {
            sema_error_at(name, "Identifier '%.*s' already in use", name->length, name->start);
            prev_at(&old->span, "Old definition was here");
        }
    }

    // Then add all types
    for (unsigned i = 0; i < active_analyser->parser->types->size; i++)
    {
        Decl *type = active_analyser->parser->types->entries[i];
        type->module = active_analyser->module;
        Decl *old = module_add_symbol(active_analyser->module, &type->name, type);
        printf("Adding %.*s\n", SPLAT_TOK(type->name));
        if (old)
        {
            sema_error_at(&type->span, "Type '%.*s' redefines identifier", SPLAT_TOK(type->name));
            prev_at(&old->span, "Old definition was here");
        }
        if (type->is_public && active_analyser->module->is_exported)
        {
            type->is_exported = true;
        }
    }

    // Then add global variables types
    for (unsigned i = 0; i < active_analyser->parser->variables->size; i++)
    {
        Decl *var = active_analyser->parser->variables->entries[i];
        printf("Added %.*s\n", SPLAT_TOK(var->span));
        assert(var->type_id == DECL_VAR);
        Decl *old = module_add_symbol(active_analyser->module, &var->name, var);
        if (old)
        {
            sema_error_at(&var->name, "Global variable '%.*s' redefines identifier", SPLAT_TOK(var->name));
            prev_at(&old->span, "Old definition was here");
        }
        if (var->is_public && active_analyser->module->is_exported)
        {
            var->is_exported = true;
        }
    }

    // Then functions
    for (unsigned i = 0; i < active_analyser->parser->functions->size; i++)
    {
        Decl *type = active_analyser->parser->functions->entries[i];

        if (type->func_decl.is_static_struct_func)
        {
            Decl *old = module_add_struct_function(active_analyser->module, &type->func_decl.full_name, type);
            if (old)
            {
                sema_error_at(&type->span, "Function '%.*s' redefines identifier", SPLAT_TOK(type->func_decl.full_name));
                prev_at(&old->span, "The old definition was here");
            }
        }
        else
        {
            Decl *old = module_add_symbol(active_analyser->module, &type->name, type);
            if (old)
            {
                sema_error_at(&type->span, "Function '%.*s' redefines identifier", SPLAT_TOK(type->name));
                prev_at(&old->span, "The old definition was here");
            }
        }

        if (type->is_public && active_analyser->module->is_exported)
        {
            type->is_exported = true;
        }
    }
}

static void analyse_variables()
{
    Vector *variables = active_analyser->parser->variables;
    for (unsigned i = 0; i < variables->size; i++)
    {
        Decl *decl = variables->entries[i];
        analyse_global_var(decl);
    }
}

static inline void init_analyser(Analyser *analyser, Module *module, Table *modules, Parser *parser)
{
    scope_init(&analyser->scope, &module->name, modules);
    analyser->module = module;
    analyser->parser = parser;
    vector_init(&analyser->labels, 16);
    vector_init(&analyser->gotos, 16);
    vector_init(&analyser->defers, 16);
}


bool analyse(Component *component, Table *modules)
{
    Vector *analysers = new_vector(128);
    for (unsigned i = 0; i < component->modules.size; i++)
    {
        Module *module = component->modules.entries[i];
        for (unsigned j = 0; j < module->files->size; j++)
        {
            Analyser *analyser = malloc_arena(sizeof(Analyser));
            init_analyser(analyser, module, modules, module->files->entries[j]);
            vector_add(analysers, analyser);
        }
    }
    unsigned analyser_count = analysers->size;

    DEBUG_LOG("1. Adding all imports");
    for (unsigned i = 0; i < analyser_count; i++)
    {
        select_analyser(analysers->entries[i]);
        add_imports();
    }

    DEBUG_LOG("2. Adding global symbols");
    for (unsigned i = 0; i < analyser_count; i++)
    {
        select_analyser(analysers->entries[i]);
        add_symbols();
    }

    if (error_found())
    {
        DEBUG_LOG("%d error(s) while adding global symbols, compile is aborted", errors());
        return false;
    }

    DEBUG_LOG("3. Analyse types");
    for (unsigned i = 0; i < analyser_count; i++)
    {
        select_analyser(analysers->entries[i]);
        analyse_types();
    }

    DEBUG_LOG("4. Analyse (global) variables");
    for (unsigned i = 0; i < analyser_count; i++)
    {
        select_analyser(analysers->entries[i]);
        analyse_variables();
    }

    if (error_found())
    {
        DEBUG_LOG("%d error(s) while resolving global types, compile is aborted", errors());
        return false;
    }

   // FATAL_ERROR("TODO");
    for (unsigned i = 0; i < analyser_count; i++)
    {
        select_analyser(analysers->entries[i]);
        check_functions();
    }


    bool success = true;

#ifdef REF

    for (unsigned i=0; i<count; i++) {
        analysers[i]->resolveTypes();
    }
    if (Diags.hasErrorOccurred()) return 1;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveTypeCanonicals();
    }
    if (errors) return errors;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveStructMembers();
    }
    if (print1) printASTs(printLib);
    if (errors) return errors;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveVars();
    }
    if (errors) return errors;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveEnumConstants();
    }
    if (errors) return errors;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveStructMembers();
    }
    if (print1) printASTs(printLib);
    if (errors) return errors;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveVars();
    }
    if (errors) return errors;

    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->resolveEnumConstants();
    }
    if (errors) return errors;

    IncrementalArrayVals ia_values;
    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->checkArrayValues(ia_values);
    }
    if (errors) return errors;

    // Set ArrayValues
    for (IncrementalArrayValsIter iter = ia_values.begin(); iter != ia_values.end(); ++iter) {
        VarDecl* D = iter->first;
        unsigned numValues = iter->second.size();
        assert(numValues);
        // NOTE: incremenal array is given InitListExpr in resolveVars()
        Expr* I = D->getInitValue();
        assert(I);
        assert(dyncast<InitListExpr>(I));
        InitListExpr* ILE = cast<InitListExpr>(I);
        Expr** values = (Expr**)context.Allocate(sizeof(Expr*)*numValues);
        memcpy(values, &iter->second[0], sizeof(Expr*)*numValues);
        ILE->setValues(values, numValues);
    }
    ia_values.clear();

    StructFunctionList structFuncs;
    for (unsigned i=0; i<count; i++) {
        errors += analysers[i]->checkFunctionProtos(structFuncs);
    }
    if (errors) return errors;
    // Set StructFunctions
    // NOTE: since these are linked anyways, just use special ASTContext from Builder
    for (StructFunctionListIter iter = structFuncs.begin(); iter != structFuncs.end(); ++iter) {
        StructTypeDecl* S = iter->first;
        const StructFunctionEntries& entries = iter->second;
        unsigned numFuncs = entries.size();
        FunctionDecl** funcs = (FunctionDecl**)context.Allocate(sizeof(FunctionDecl*)*numFuncs);
        memcpy(funcs, &entries[0], sizeof(FunctionDecl*)*numFuncs);
        S->setStructFuncs(funcs, numFuncs);
    }

    for (unsigned i=0; i<count; i++) {
        analysers[i]->checkVarInits();
    }
    if (print2) printASTs(printLib);
    if (Diags.hasErrorOccurred()) return 1;

    for (unsigned i=0; i<count; i++) {
        analysers[i]->checkFunctionBodies();
    }
    if (Diags.hasErrorOccurred()) return 1;

    for (unsigned i=0; i<count; i++) {
        analysers[i]->checkDeclsForUsed();
    }

    if (print3) printASTs(printLib);
    return errors;
}
    /*
    for (int i = 0; i < parser->types->size; i++)
    {
        success = success & register_type(parser.types->entries[i]);
    }
    for (int i = 0; i < parser.functions->size; i++)
    {
        success = success & register_function(parser.functions->entries[i]);
    }
*/
    // analyse_module(ast->source.module);
#endif
    return !success;
}
