#include <string.h>
#include "diagnostics.h"
#include "semantic_analysis.h"
#include "vector.h"
#include "ast_utils.h"
#include "vector.h"
#include "ast_types.h"
#include "parser.h"
#include "error.h"
#include "table.h"
#include "arena_allocator.h"
#include "scope.h"
#include "attributes.h"
#include "constant_folding.h"

typedef struct _Analyser
{
    Module *module;
    Parser *parser;
    Scope scope;
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

Analyser analyser;

/*
static bool fold_expression(Ast *expr);

static bool fold_unary(Ast *expr)
{
    assert(expr->type == AST_UNARY_EXPR && "Unary only");
    Ast *right = expr->unaryExpr.right;
    if (!fold_expression(right)) return false;
    switch (expr->unaryExpr.operator)
    {
        case TOKEN_MINUS:
            switch (right->type)
            {
                case AST_INT_EXPR:
                    if (right->integerExpr.sign)
                    {
                        expr->integerExpr.i = -right->integerExpr.i;
                    }
                    else
                    {
                        expr->integerExpr.i = -(int64_t)right->integerExpr.u;
                    }
                    expr->integerExpr.sign = 1;
                    expr->type = AST_INT_EXPR;
                    return true;
                case AST_FLOAT_CONST:
                    expr->type = AST_FLOAT_CONST;
                    expr->floatExpr.f = -right->floatExpr.f;
                    return true;
                case AST_BOOL_EXPR:
                    expr->type = AST_INT_EXPR;
                    expr->integerExpr.sign = 1;
                    expr->integerExpr.i = right->boolExpr.i ? -1 : 0;
                    return true;
                default:
                    assert(false && "Can't happen");
                    return false;
            }
        case TOKEN_AND:
            return false;
        case TOKEN_BIT_NOT:
            switch (right->type)


    }
}
static bool fold_expression(Ast *expr)
{
    switch (expr->type)
    {
        case AST_INT_EXPR:
        case AST_FLOAT_CONST:
        case AST_BOOL_EXPR:
            return true;
        case AST_UNARY_EXPR:
            return fold_unary(expr);
        case AST_BINARY_EXPR:
            // Use bitwise or to evaluate both.
            if (!fold_expression(expr->binaryExpr.right) | !fold_expression(expr->binaryExpr.left)) return false;
            return fold_binary(expr);
        case AST_IDENTIFIER_EXPR:
            // Change this if identifier is constant
            // TODO
            return false;
        case AST_ACCESS_EXPR:
            fold_expression(expr->accessExpr.parent);
            fold_expression(expr->accessExpr.sub_element);
            // Or??? What about Module.constant?? TODO
            return false;
        case AST_SUBSCRIPT_EXPR:
        case AST_CALL_EXPR:
        case AST_DESIGNATED_INITIALIZED_EXPR:
        case AST_STRUCT_INIT_VALUES_EXPR:
            return false;
        default:
            assert(false && "Attempted to fold non-expression");
            return false;

    }
}
*/
/*
bool convert_to_const_int(Ast *expr)
{
    if (expr->type == AST_INT_EXPR) return true;
    if (expr->type == AST_BOOL_EXPR)
    {
        expr->integerExpr.u = expr->boolExpr.i ? 1 : 0;
        expr->integerExpr.sign = 0;
        return true;
    }
    return false;
}

void error_at_ast(Ast *ast, char *text)
{}

void error_at_token(Token token, char *text)
{}

void resolve_unresolved_type(Ast *type)
{
    // TODO
}

void analyze_expression(Ast *expression)
{}

void resolve_type(Ast *type)
{
    switch (type->type)
    {
        case AST_UNSIZED_ARRAY_TYPE:
        case AST_SIZED_ARRAY_TYPE:
        case AST_PTR_TYPE:
        case AST_BASIC_TYPE:
            return;
        case AST_UNRESOLVED_TYPE:
            resolve_unresolved_type(type);
            return;
        case AST_UNRESOLVED_PTR_TYPE:
            resolve_type(type->ptrType.baseType);
            type->type = AST_PTR_TYPE;
            return;
        case AST_UNRESOLVED_UNSIZED_ARRAY_TYPE:
            resolve_type(type->unsizedArrayType.baseType);
            type->type = AST_UNSIZED_ARRAY_TYPE;
            return;
        case AST_UNRESOLVED_SIZED_ARRAY_TYPE:
        {
            analyze_expression(type->unresolvedSizedArrayType.sizeExpr);
            uint64_t size = 0;
            if (!convert_to_const_int(type->unresolvedSizedArrayType.sizeExpr))
            {
                error_at_ast(type->unresolvedSizedArrayType.sizeExpr, "Could not resolve constant size");
            }
            else if (type->unresolvedSizedArrayType.sizeExpr->integerExpr.sign)
            {
                int64_t i = type->unresolvedSizedArrayType.sizeExpr->integerExpr.i;
                if (i < 0)
                {
                    error_at_ast(type->unresolvedSizedArrayType.sizeExpr, "Cannot have negative sizes");
                    size = 0;
                }
                else
                {
                    size = (uint64_t)i;
                }
            }
            else
            {
                size = type->unresolvedSizedArrayType.sizeExpr->integerExpr.u;
            }
            resolve_type(type->unresolvedSizedArrayType.baseType);
            type->sizedArrayType.baseType = type->unresolvedSizedArrayType.baseType;
            type->sizedArrayType.size = size;
            type->type = AST_SIZED_ARRAY_TYPE;
            return;
        }
        default:
            assert(false && "Unexpected type");
    }
}
bool analyse_function_decl(Ast *function_decl)
{
    assert(function_decl->type == AST_FUNC_DECL && "Expected function decl");
    resolve_type(function_decl->func_decl.r_type);
    return true;
}
bool analyse_function(Ast *function)
{
    assert(function->type == AST_FUNC_DEFINTION && "Expected function");
    analyse_function_decl(function->func_definition.definition);
    return true;
}

static inline bool strtokencmp(Token *token, char *chars)
{
    size_t len = strlen(chars);
    if (len != token->length) return false;
    return memcmp(token->start, chars, len) == 0;
}
/*

bool analyse_module(Ast *module, char *module_file)
{
    Token name = module->module.name;
    if (name.length > 1 && name.start[0] == '_' && name.start[1] == '_')
    {
        error_at_token(name, "Invalid module name");
        return false;
    }
    if (!is_lower(name.start[0]))
    {
        error_at_token(name, "Module name must start with lower case");
        return false;
    }
    if (strtokencmp(&name, "c2"))
    {
        error_at_token(name, "Module name cannot be 'c2'");
        return false;
    }
    if (strtokencmp(&name, "main"))
    {
        error_at_token(name, "Module name cannot be 'main'");
    }


    // First create Module, then AST, the get Context (from Module?)
    const char* name = Context.addIdentifier(name_, strlen(name_));
    ast.setName(name, loc);

    if (module) {
        // for external modules, filename should match module name
        if (module->getName() != name) {
            Diag(loc, diag::err_file_wrong_module) << module->getName() << name;
        }
    } else {
        module = component.getModule(name);
    }
    module->addAST(&ast);

    MEM_DECL(DECL_IMPORT);
    ImportDecl* U = new (Context) ImportDecl(name, loc, true, name, SourceLocation());
    U->setType(Context.getModuleType(U));
    U->setUsed();
    ast.addImport(U);
    addSymbol(U);
}*/

Table symbols;

static bool register_type_named(Token *name, Ast *type)
{
    Ast *previous = table_set(&symbols, name->start, name->length, type);
    if (previous)
    {
        error_at(name, "Name already used");
        error_at(&previous->span, "Previous was found here");
        return false;
    }
    return true;

}

static Ast *get_symbol(Analyser *analyser, Token *token, bool is_type, bool used_public)
{
    return scope_find_symbol(&analyser->scope, token, is_type, used_public);
}

static bool resolve_type(Analyser *analyser, Ast *type_expr, bool used_public)
{
    if (type_expr->type_expr.flags.resolved)
    {
        return true;
    }
    bool resolves = false;
    switch (type_expr->type_expr.type)
    {
        case TYPE_EXPR_IDENTIFIER:
        {
            // TODO module symbol
            Token *name = &type_expr->type_expr.identifier_type_expr.name;
            Ast *symbol = get_symbol(analyser, name, true, used_public);
            if (!symbol) return false;
            if (symbol->type != AST_TYPE_DEFINITION)
            {
                error_at(&type_expr->span, "Unknown type '%.*s'", name->length, name->start);
                return false;
            }
            type_expr->type_expr.identifier_type_expr.resolved_type = symbol;
            resolves = true;
            break;
        }
        case TYPE_EXPR_ARRAY:
            resolves = resolve_type(analyser, type_expr->type_expr.array_type_expr.type, used_public);
            // TODO also evaluate size.
            break;
        case TYPE_EXPR_POINTER:
            resolves = resolve_type(analyser, type_expr->type_expr.pointer_type_expr.type, used_public);
            break;
        case TYPE_EXPR_VOID:
            resolves = true;
            break;
        default:
            FATAL_ERRORF("Unsupported type expr");
    }
    type_expr->type_expr.flags.resolved = resolves;
    return resolves;
}



static bool register_function(Ast *function)
{
    switch (function->type)
    {
        case AST_FUNC_DECL:
            break;
        case AST_FUNC_DEFINTION:
            function = function->func_definition.func_decl;
            break;
        default:
            FATAL_ERRORF("This should never happen");
    }
/*    Ast *old = table_set(&symbols, function->func_decl.name->span.start, function->func_decl.name->span.length, function);

    if (old)
    {
        sema_error_at(&function->func_decl.name->span, "Redefinition of symbol");
        sema_error_at(&old->span, "Previous definition was here");
        return false;
    }*/
    FATAL_ERRORF("Broken");
    return true;
}



static void register_alias(Analyser *analyser, const char *name, const char *alias)
{
    FATAL_ERRORF("TODO");
}

static inline void add_imports(Analyser *analyser)
{
    for (unsigned i = 0; i < analyser->parser->imports->size; i++)
    {
        scope_add_import_declaration(&analyser->scope, analyser->parser->imports->entries[i]);
    }
}

unsigned check_attributes(Ast *ast_attribute_list)
{
    if (!ast_attribute_list) return 0;
    unsigned errors = 0;
    assert(ast_attribute_list->type == AST_ATTRIBUTE_LIST);
    Vector *attributes = ast_attribute_list->attribute_list.list;
    for (unsigned a = 0; a < attributes->size; a++)
    {
        // Improve: check that same attribute does not appear more than once?
        Ast *attribute = attributes->entries[a];
        assert(attribute->type == AST_ATTRIBUTE);
        Token *name = &attribute->attribute.name;
        AttributeType attribute_type = attribute_type_from_token(name);
        if (attribute_type == ATTRIBUTE_UNKNOWN)
        {
            sema_error_at(&attribute->attribute.name, "Unknown attribute '%.*s'", name->length, name->start);
            errors++;
            continue;
        }
        const AttributeInfo *info = attribute_info_from_type(attribute_type);
        Ast *value = attribute->attribute.value;
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
                if (value->type != AST_STRING_EXPR)
                {
                    sema_error_at(&value->span, "Expected a string argument");
                    errors++;
                }
                break;
            case ATTR_ARG_UINT:
                switch (value->type)
                {
                    case AST_UINT_EXPR:
                        continue;
                    case AST_INT_EXPR:
                        if (value->int_expr.i >= 0) continue;
                        break;
                    default:
                        break;
                }
                sema_error_at(&value->span, "Expected a unsigned int value");
                errors++;
                break;
            case ATTR_ARG_INT:
                if (value->type != AST_INT_EXPR && value->type != AST_UINT_EXPR)
                {
                    sema_error_at(&value->span, "Expected an integer value");
                    errors++;
                }
                break;
            case ATTR_ARG_NUMBER:
                if (value->type != AST_INT_EXPR && value->type != AST_FLOAT_EXPR && value->type != AST_UINT_EXPR)
                {
                    sema_error_at(&value->span, "Expected a numeric value");
                    errors++;
                }
                break;
            case ATTR_ARG_BOOL:
                if (value->type != AST_BOOL_EXPR)
                {
                    sema_error_at(&value->span, "Expected a boolean value");
                    errors++;
                }
                break;
            default:
                FATAL_ERRORF("Unexpected value");
        }
    }
    return errors;
}

static unsigned analyse_struct_members(Analyser *analyser, Ast *type, Table *names, Vector *members, bool used_public)
{
    unsigned errors = 0;
    for (unsigned  i = 0; i < members->size; i++)
    {
        Ast *member = members->entries[i];
        Token *name = &member->struct_member.name;
        if (name->length == 0)
        {
            errors += analyse_struct_members(analyser, member, names, member->struct_member.members, used_public);
            continue;
        }
        Ast *old_member = table_set_token(names, name, member);
        if (old_member)
        {
            sema_error_at(name, "Duplicate member '%.*s'", name->length, name->start);
            sema_error_at(&old_member->span, "Previous definition was here");
            errors++;
            continue;
        }
        if (member->struct_member.type != STRUCT_MEMBER_TYPE_NORMAL)
        {
            Table table;
            table_init(&table, 4);
            analyse_struct_members(analyser, member, &table, member->struct_member.members, used_public);
        }
        else
        {
            if (!resolve_type(analyser, member->struct_member.value_type, used_public))
            {
                errors++;
            }
        }
    }
    return errors;
}

unsigned check_type(Analyser *analyser, Ast *type)
{
    switch (type->definition.definition_type)
    {
        case BUILTIN_TYPE:
            return 0;
        case STRUCT_TYPE:
        {
            Table names;
            table_init(&names, 4);
            return analyse_struct_members(analyser, type, &names, type->definition.def_struct.members, type->definition.is_public);
        }
        case ALIAS_TYPE:
            return resolve_type(analyser, type->definition.def_alias.type_definition, type->definition.is_public) ? 0 : 1;
        case ENUM_TYPE:
            FATAL_ERRORF("TODO");
        case FUNC_TYPE:
            FATAL_ERRORF("TODO");
        default:
            FATAL_ERRORF("Invalid type");
    }
}
unsigned check_types(Analyser *analyser)
{
    unsigned errors = 0;
    // Then add all types
    for (unsigned i = 0; i < analyser->parser->types->size; i++)
    {
        Ast *type = analyser->parser->types->entries[i];
        assert(type->definition.module);
        errors += check_type(analyser, type);
        check_attributes(type->var_definition.attributes);
    }
#ifdef TODO
    switch (an
    switch (type->type_expr.type)
    {
        case TYPE_EXPR_ARRAY:
            evaluate_constant(type->type_expr.array_type_expr.size);
            resolve_type(analyser, type->type_expr.array_type_expr.type);
            break;
        case TYPE_EXPR_POINTER:
            resolve_type(analyser, type->type_expr.pointer_type_expr.type);
            break;
        case TYPE_EXPR_VOID:
            return;
        case TYPE_EXPR_IDENTIFIER
            resolve_type(type->alias_type.type_definition);
            type->alias_type.alias
        case AST_BUILTIN_TYPE:
        case AST_ENUM_TYPE:
        case AST_STRUCT_TYPE:
        case AST_FUNC_TYPE:

    }
    assert(type->type = AST_TERNARY_EXPR)
    // check generic type
    type = type
    unsigned errors = TR->checkType(D->getType(), D->isPublic());

    // check extra stuff depending on subclass
    switch (D->getKind()) {
    case DECL_FUNC:
    case DECL_VAR:
    case DECL_ENUMVALUE:
        FATAL_ERROR("Cannot have type");
        break;
    case DECL_ALIASTYPE:
        // Any UnresolvedType should point to decl that has type set
        if (errors == 0) {
            AliasType* A = cast<AliasType>(D->getType().getTypePtr());
            QualType Q = TR->resolveUnresolved(A->getRefType());
            A->updateRefType(Q);
        }
        break;
    case DECL_STRUCTTYPE:
    {
        Names names;
        StructTypeDecl* S = cast<StructTypeDecl>(D);
        analyseStructNames(S, names, S->isStruct());
        break;
    }
    case DECL_ENUMTYPE:
    {
        EnumTypeDecl* E = cast<EnumTypeDecl>(D);
        if (E->numConstants() == 0) {
            Diags.Report(D->getLocation(), diag::err_empty_enum) << D->getName();
            errors++;
        }
        break;
    }
    case DECL_FUNCTIONTYPE:
    {
        const FunctionTypeDecl* FTD = cast<FunctionTypeDecl>(D);
        // set module on inner FunctionDecl
        FTD->getDecl()->setModule(FTD->getModule());
        // dont check return/argument types yet
        break;
    }
    case DECL_ARRAYVALUE:
    case DECL_IMPORT:
    case DECL_LABEL:
        FATAL_ERROR("Cannot have type decl");
        break;
    }
#endif
    return errors;
}


static inline void add_symbols(Analyser *analyser)
{
    // First, register imports
    for (unsigned i = 0; i < analyser->parser->imports->size; i++)
    {
        Ast *import = analyser->parser->imports->entries[i];
        assert(import->type == AST_IMPORT);
        Token *name;
        switch (import->import.type)
        {
            case IMPORT_TYPE_LOCAL:
            case IMPORT_TYPE_FULL:
                name = &import->import.module_name;
                break;
            case IMPORT_TYPE_ALIAS:
                name = &import->import.alias;
                break;
            default:
                FATAL_ERRORF("Unknown type");
        }
        Ast *old = module_add_symbol(analyser->module, name, import);
        if (old)
        {
            sema_error_at(name, "Identifier '%.*s' already in use", name->length, name->start);
            sema_error_at(&old->span, "Old definition was here");
        }
    }

    // Then add all types
    for (unsigned i = 0; i < analyser->parser->types->size; i++)
    {
        Ast *type = analyser->parser->types->entries[i];
        type->definition.module = analyser->module;
        assert(type->type == AST_TYPE_DEFINITION);
        Ast *old = module_add_symbol(analyser->module, &type->definition.name, type);
        if (old)
        {
            sema_error_at(&type->definition.name, "Type '%.*s' redefines identifier", type->definition.name.length, type->definition.name.start);
            sema_error_at(&old->span, "Old definition was here");
        }
        if (type->definition.is_public && analyser->module->is_exported)
        {
            type->definition.is_exported = true;
        }
    }

    // Then add global variables types
    for (unsigned i = 0; i < analyser->parser->variables->size; i++)
    {
        Ast *var = analyser->parser->variables->entries[i];
        assert(var->type == AST_VAR_DEFINITION);
        Ast *old = module_add_symbol(analyser->module, &var->var_definition.name, var);
        if (old)
        {
            sema_error_at(&var->var_definition.name, "Global variable '%.*s' redefines identifier", &var->var_definition.name);
            sema_error_at(&old->span, "Old definition was here");
        }
        if (var->var_definition.is_public && analyser->module->is_exported)
        {
            var->var_definition.is_exported = true;
        }
    }

    // Then functions
    for (unsigned i = 0; i < analyser->parser->functions->size; i++)
    {
        Ast *type = analyser->parser->functions->entries[i];
        FunctionName *name = type->func_definition.func_decl->func_decl.name;

        if (name->struct_name.length)
        {
            Ast *old = module_add_struct_function(analyser->module, &name->full_name, type);
            if (old)
            {
                sema_error_at(&type->func_definition.func_decl->span, "Function '%.*s' redefines identifier", name->full_name.length, name->full_name.start);
                sema_error_at(&old->func_definition.func_decl->span, "The old definition was here");
            }
        }
        else
        {
            Ast *old = module_add_symbol(analyser->module, &name->function_name, type);
            if (old)
            {
                sema_error_at(&name->function_name, "Function '%.*s' redefines identifier", name->full_name.length, name->full_name.start);
                sema_error_at(&old->func_definition.func_decl->span, "The old definition was here");
            }
        }


        if (type->func_definition.is_public && analyser->module->is_exported)
        {
            type->func_definition.is_exported = true;
        }
    }
}

static inline void init_analyser(Analyser *analyser, Module *module, Table *modules, Parser *parser)
{
    scope_init(&analyser->scope, &module->name, modules);
    analyser->module = module;
    analyser->parser = parser;
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
    for (unsigned i = 0; i < analyser_count; i++)
    {
        Analyser *analyser = analysers->entries[i];
        add_imports(analyser);
    }

    for (unsigned i = 0; i < analyser_count; i++)
    {
        Analyser *analyser = analysers->entries[i];
        add_symbols(analyser);
    }

    for (unsigned i = 0; i < analyser_count; i++)
    {
        Analyser *analyser = analysers->entries[i];
        check_types(analyser);
    }


    bool success = true;
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
    return success;
}
