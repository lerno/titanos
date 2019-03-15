//
// Created by Christoffer Lernö on 2019-03-05.
//

#include <string.h>
#include "semantic_analyser.h"
#include "module.h"
#include "scope.h"
#include "analyser.h"
#include "diagnostics.h"
#include "component.h"
#include "error.h"
#include "static_array.h"
#include "parsing.h"
#include "decl.h"
#include "expr.h"
#include "expression_analysis.h"

#define MAX_FUNCTIONS 0xFFFF
#define MAX_VARIABLES 0xFFFF
#define MAX_VARIABLE_ADD 0x0FFF
#define MAX_IMPORTS 0x7FFF

typedef struct _VariableAdd {
    SourceRange range;
    Token variable_name;
} VariableAdd;

typedef struct _SemanticContext
{
    /*
    unsigned remainder : 31;
    const char *current_module;
    SourceRange module_def;
    Decl *module;
    Vector *types; // Decl *
    Vector *array_values; // Remember to add the module to these!
     */
    bool is_interface;
    const char *filename;
    Vector macros;
    Vector enum_values;
    Vector array_values;
    Component *component;
    Parser *parser;
    Module *module;
    Scope scope;
    STATIC_ARRAY(Decl *, MAX_IMPORTS) imports;
    STATIC_ARRAY(Decl *, MAX_VARIABLES) variables;
    STATIC_ARRAY(Decl *, MAX_FUNCTIONS) functions;
    STATIC_ARRAY(VariableAdd, MAX_VARIABLE_ADD) variable_add;
    Decl *current_func;
    Vector labels;
    Vector gotos;
    Vector defers;
    Expr *call_stack[CALL_STACK_DEPTH];
    unsigned call_stack_current;
    /*
     *   AST& ast;
         const Module& module;
         std::unique_ptr<Scope> globals;
         std::unique_ptr<TypeResolver> TR;
         c2lang::DiagnosticsEngine& Diags;
         FunctionAnalyser functionAnalyser;
         bool verbose;
     */
} SemanticContext;

static bool sema_analyse_variable(Decl *variable);

__thread static SemanticContext *context;

void init_semantic_analyser(Component *component, Module *module, const char *filename, bool is_interface)
{
    if (!context)
    {
        context = malloc(sizeof(SemanticContext));
        vector_init(&context->macros, 64);
        vector_init(&context->enum_values, 64);
        context->scope.module = NULL;
    }
    else
    {
        context->macros.size = 0;
        context->enum_values.size = 0;
        context->scope.module = (void *)0x01;
    }
    // TODO remove

    STATIC_ARRAY_CLEAR(context->imports);
    STATIC_ARRAY_CLEAR(context->variables);
    STATIC_ARRAY_CLEAR(context->functions);
    STATIC_ARRAY_CLEAR(context->variable_add);
    context->is_interface = is_interface;
    context->filename = filename;
    context->call_stack_current = 0;
    context->component = component;
    context->module = module;
    context->parser = NULL;
    // Decl *
//    parser->current_module = NULL;
 //   parser->filename = filename;
  //  parser->array_values = new_vector(32);
    active_scope = &context->scope;
}

static void add_import(Decl *import)
{
    assert(import->type_id == DECL_IMPORT);
    const char *name;

    STATIC_ARRAY_ADD(context->imports, import);
    switch (import->import.type)
    {
        case IMPORT_TYPE_LOCAL:
        case IMPORT_TYPE_FULL:
            name = import->name;
            break;
        case IMPORT_TYPE_ALIAS:
            name = import->import.alias;
            break;
        default:
            UNREACHABLE
    }
    Decl *old = module_add_symbol(context->module, name, import);
    if (old)
    {
        // TODO point at name?
        sema_error_at(import->span, "Identifier '%s' already in use", name);
        prev_at(old->span, "Old definition was here");
    }
    import->module = context->module;
    scope_add_import_declaration(import);

    STATIC_ARRAY_ADD(context->imports, import);
}


void sema_add_import(SourceRange start, Token import, Token alias, ImportType type)
{
    if (type == XTYPE_ALIAS)
    {
        if (alias.string == context->module->name)
        {
            error_at(alias.span, "Cannot set alias to the current module");
            return;
        }
        if (!is_lower(alias.string))
        {
            error_at(alias.span, "Alias must start with a lower case letter");
            return;
        }
        if (alias.span.length > MAX_IDENTIFIER_LENGTH)
        {
            error_at(alias.span, "Alias name may not exceed %d characters", MAX_IDENTIFIER_LENGTH);
            return;
        }
        // IMPROVE prevent alias from being 'main' or 'c2'?
    }
    if (import.string == context->module->name)
    {
        error_at(import.span, "Cannot import the current module");
        return;
    }
    Decl *import_decl = decl_new(DECL_IMPORT, start, import.string, false);
    import_decl->import.type = type;
    import_decl->import.alias = import.string;
    add_import(import_decl);

}

void sema_add_array_entry(Token name, SourceRange range)
{
    STATIC_ARRAY_ADD(context->variable_add, ((VariableAdd) { .range = range, .variable_name = name }));
}


void sema_add_module(SourceRange start, Token name)
{
    if (context->module)
    {
        if (context->module->name != name.string)
        {
            // for external modules, filename should match module name
            sema_error_at(name.span, "Wrong module '%s' expected '%s'", context->module->name);
        }
    }
    else
    {
        context->module = component_get_module(context->component, name.string);
    }
    Decl *import = decl_new(DECL_IMPORT, start, name.string, false);
    import->is_used = true;
    import->import.type = IMPORT_TYPE_LOCAL;
    import->is_public = true;
    if (name.span.length > 1 && name.string[0] == '_' && name.string[1] == '_')
    {
        error_at(name.span, "Invalid module name '%s'", name.string);
    }
    if (!is_lower(name.string))
    {
        error_at(name.span, "Module must start with lower case");
    }
    if (strcmp(name.string, "c2") == 0)
    {
        error_at(name.span, "'c2' module name is reserved");
    }
    if (strcmp(name.string, "main") == 0)
    {
        error_at(name.span, "'main' module name is reserved");
    }
    if (name.span.length > MAX_IDENTIFIER_LENGTH)
    {
        error_at(import->span, "Module name '%s' exceeds max length %d", MAX_IDENTIFIER_LENGTH);
    }
    context->parser = malloc(sizeof(Parser));
    init_parser(context->parser, context->filename, context->is_interface);
    STable *table = malloc(sizeof(STable));
    stable_init(table, 32);
    stable_set(table, name.string, context->module);
    scope_init(&context->scope, context->module->name, table);
    //TODO Ast
    vector_add(context->module->files, context->parser);
    add_import(import);

}

static inline bool is_both_const(Expr *left, Expr *right)
{
    return left->expr_id == EXPR_CONST && right->expr_id == EXPR_CONST;
}


static Decl *add_unparsed_type(DeclType type, bool public, SourceLoc loc, Token *name)
{
    if (context->is_interface)
    {
        if (public) sema_error_at2(loc, "'public' used in interface");
        public = true;
    }

    Decl *decl = decl_new2(type, loc, name, public);
    decl->is_unparsed = true;
    Decl *old = module_add_symbol(context->module, name->string, decl);
    if (old)
    {
        sema_error_at(name->span, "Identifier '%s' already in use", name->string);
        prev_at(old->name_span, "Previous use was here");
    }
    else
    {
        vector_add(context->parser->types, decl);
    }
    return decl;
}

void sema_add_func_definition(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_type,
                              SourceRange unparsed_parameters, SourceRange unparsed_body, bool has_body)
{
    DEBUG_LOG("Add function '%s'", name->string);
    if (context->is_interface)
    {
        if (has_body)
        {
            sema_error_at2(unparsed_body.loc, "Function in interface cannot have a function body");
            return;
        }
    }
    else
    {
        if (!has_body)
        {
            sema_error_at2(unparsed_body.loc, "Function '%s' is missing function body", name);
            return;
        }
    }
    Decl *decl = decl_new(DECL_FUNC, name->span, name->string, is_public);
    decl_add_own_type(decl, TYPE_FUNC);
    decl->func_decl.unparsed.rtype = unparsed_type;
    decl->attributes = attributes;
    decl->func_decl.unparsed.body = unparsed_body;
    decl->func_decl.unparsed.params = unparsed_parameters;
}

void sema_add_var_definition(bool is_public, SourceLoc loc, Token *name, SourceRange unparsed_type, Vector *attributes, SourceRange unparsed_init)
{
    DEBUG_LOG("Add variable '%s'", name->string);
    Decl *decl = decl_new(DECL_VAR, name->span, name->string, is_public);
    decl->is_unparsed = true;
    decl->var.kind = VARDECL_GLOBAL;
    decl->var.unparsed.type_range = unparsed_type;
    decl->var.unparsed.init_range = unparsed_init;
    decl->attributes = attributes;
    if (is_public && context->module->is_exported)
    {
        decl->is_exported = true;
    }
    // TODO, I don't think this is correct. What about local variables? They should be ok if they are in another file!
    module_add_symbol(context->module, name->string, decl);
    vector_add(context->parser->variables, decl);
}


void sema_add_enum(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_type,
                   SourceRange unparsed_body)
{
    DEBUG_LOG("Add enum '%s'", name->string);
    Decl *decl = add_unparsed_type(DECL_ENUM_TYPE, is_public, loc, name);
    decl_add_own_type(decl, TYPE_ENUM);
    decl->enum_decl.unparsed.body = unparsed_body;
    decl->enum_decl.unparsed.type = unparsed_type;
    decl->attributes = attributes;
}

void sema_add_struct(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_body)
{
    DEBUG_LOG("Add struct '%s'", name->string);
    Decl *decl = add_unparsed_type(DECL_STRUCT_TYPE, is_public, loc, name);
    decl_add_own_type(decl, TYPE_STRUCT);
    decl->attributes = attributes;
    decl->struct_decl.unparsed.body = unparsed_body;
}

void sema_add_union(bool is_public, SourceLoc loc, Token *name, Vector *attributes, SourceRange unparsed_body)
{
    DEBUG_LOG("Add union '%s'", name->string);
    Decl *decl = add_unparsed_type(DECL_UNION_TYPE, is_public, loc, name);
    decl_add_own_type(decl, TYPE_UNION);
    decl->attributes = attributes;
    decl->struct_decl.unparsed.body = unparsed_body;
}

void sema_add_alias(bool is_public, SourceLoc loc, Token *name, SourceRange unparsed_type)
{
    DEBUG_LOG("Add alias '%s'", name->string);
    Decl *decl = add_unparsed_type(DECL_ALIAS_TYPE, is_public, loc, name);
    decl->unparsed_alias = unparsed_type;
}

void sema_add_func_type(bool is_public, SourceLoc loc, Token *name, SourceRange unparsed_type)
{
    DEBUG_LOG("Add func-type '%s'", name->string);
    Decl *decl = add_unparsed_type(DECL_FUNC_TYPE, is_public, loc, name);
    TODO
//    decl->unparsed_type = unparsed_type;
}

QualifiedType sema_resolve_prefixed_type(Token module, Token type_name)
{
    TODO
}

static void sema_analyse_type(Decl *type);

Expr *sema_create_identifier_expr(Token token)
{
    Decl *decl = scope_find_symbol(token.string, false, false, token.span);
    if (!decl) return NULL;
    if (decl->type_id != DECL_VAR)
    {
        sema_error_at(token.span, "Expected a variable");
        // TODO
        return NULL;
    }
    // DO const inlining TODO
    if (!sema_analyse_variable(decl)) return NULL;
    // TODO check type
    // TODO check
    if (IS_CONST(decl->var.original_type) && decl->var.init_expr && decl->var.init_expr->expr_id == EXPR_CONST)
    {
        return decl->var.init_expr;
    }
    Expr *expr = expr_new(EXPR_IDENTIFIER, prev_tok.span);
    expr->identifier_expr.resolved = decl;
    expr->type = decl->type;
    return expr;

}


QualifiedType sema_resolve_type(Token type_name, bool public_use)
{
    Decl *decl = scope_find_symbol(type_name.string, true, public_use, type_name.span);
    if (!decl) return InvalidType;
    if (!decl_is_type(decl))
    {
        sema_error_at(type_name.span, "A type is required here");
        return InvalidType;
    }
    switch (decl->type_id)
    {
        case DECL_BUILTIN:
        case DECL_ALIAS_TYPE:
        case DECL_STRUCT_TYPE:
        case DECL_UNION_TYPE:
        case DECL_ENUM_TYPE:
        case DECL_FUNC_TYPE:
            return decl->self_type;
        case DECL_FUNC:
        case DECL_VAR:
        case DECL_ENUM_CONSTANT:
        case DECL_ARRAY_VALUE:
        case DECL_IMPORT:
        case DECL_LABEL:
        case DECL_MACRO:
        case DECL_MACRO_PARAM:
            UNREACHABLE
    }
    UNREACHABLE
}

static inline bool sema_analyse_func_decl(Decl *func_decl, bool type_only)
{
    SourceRange rtype_range = func_decl->func_decl.unparsed.rtype;
    SourceRange params_range = func_decl->func_decl.unparsed.params;
    SourceRange body_range = func_decl->func_decl.unparsed.body;
    push_lexer(&rtype_range);
    QualifiedType return_type = parse_type(func_decl->is_public);
    if (IS_INVALID(return_type))
    {
        pop_lexer();
        return false;
    }
    Token token = parse_possible_struct_prefix();
    if (token.type == TOKEN_ERROR)
    {
        pop_lexer();
        return false;
    }
    if (expect_eof(pop_lexer(), "Expected function name")) return false;
    if (token.type == TOKEN_IDENTIFIER)
    {
        const char *struct_name = token.string;
        size_t len = token.span.length + strlen(func_decl->name) + 1;
        assert(len < 127);
        char buffer[128];
        // Improve
        snprintf(buffer, len, "%s.%s", func_decl->name, token.string);
        func_decl->func_decl.full_name = symtab_add(buffer, (uint16_t)len);
        func_decl->func_decl.is_struct_func = true;
    }

    LEXER_BEGIN(&params_range)
    LEXER_EVAL = parse_argument_list(func_decl, func_decl->is_public, type_only)
    LEXER_END

    if (func_decl->attributes->size)
    {
        TODO
    }
    // TODO parse body
    return true;
}

static inline bool sema_analyse_alias(Decl *alias_decl)
{
    DEBUG_LOG("Analyse alias %s", alias_decl->name);
    assert(alias_decl->is_unparsed && alias_decl->is_being_parsed);
    alias_decl->alias = parse_type_in_range(&alias_decl->unparsed_alias, alias_decl->is_public);
    return !IS_INVALID(alias_decl->alias);
}

static inline bool sema_analyse_struct(Decl *decl)
{
    DEBUG_LOG("Analyse struct %s", decl->name);
    assert(decl->is_unparsed && decl->is_being_parsed);

    if (!parse_struct_body(decl, decl->struct_decl.unparsed.body)) return false;

    decl->struct_decl.is_global = true;

    // TODO handle attributes
    return true;
}

static inline bool sema_analyse_union(Decl *decl)
{
    assert(decl->is_unparsed && decl->is_being_parsed);

    if (!parse_struct_body(decl, decl->struct_decl.unparsed.body)) return false;

    decl->struct_decl.is_global = true;

    // TODO handle attributes
    return false;
}

static inline bool sema_analyse_enum_type(Decl *decl)
{
    assert(decl->is_unparsed && decl->is_being_parsed);
    decl->is_being_parsed = true;
    QualifiedType type = parse_type_in_range(&decl->enum_decl.unparsed.type, decl->is_public);
    if (IS_INVALID(type)) return false;
    if (!type_is_int(type.type))
    {
        sema_error_at(decl->enum_decl.unparsed.type, "An integer value is required");
        return false;
    }
    // TODO check attributes
    decl->enum_decl.type = type;
    LEXER_BEGIN(&decl->enum_decl.unparsed.body)
    LEXER_EVAL = parse_enum_body(decl);
    LEXER_END
    return true;
}

Expr *sema_expr_const_int(SourceRange span, Value value)
{
    Expr *number = expr_new(EXPR_CONST, span);
    number->type = type_compint();
    number->const_expr.value = value;
    return number;
}

static inline Expr *binexpr(BinOp op, Expr *left, Expr *right)
{
    Expr *binary = expr_new(EXPR_BINARY, left->span);
    binary->binary_expr.left = left;
    binary->binary_expr.right = right;
    binary->binary_expr.operator = op;
}

static inline Expr *sema_expr_mult(Expr *left, Expr *right)
{
    if (!perform_arithmetic_conversions(left, right, false, true)) return NULL;
    if (is_both_const(left, right))
    {
        // TODO handle error
        left->const_expr.value = value_mult(left->const_expr.value, right->const_expr.value);
        return left;
    }
    return binexpr(BINOP_MULT, left, right);
}

static inline Expr *sema_expr_add(Expr *left, Expr *right)
{
    // TODO handle pointers
    if (!perform_arithmetic_conversions(left, right, false, true)) return NULL;
    if (is_both_const(left, right))
    {
        left->const_expr.value = value_add(left->const_expr.value, right->const_expr.value);
        if (left->const_expr.value.type == VALUE_TYPE_ERROR)
        {
            FATAL_ERROR("Value folding failed");
        }
        return left;
    }
    return binexpr(BINOP_ADD, left, right);
}

static inline Expr *sema_expr_sub(Expr *left, Expr *right)
{
    // TODO handle pointers
    if (!perform_arithmetic_conversions(left, right, false, true)) return NULL;
    if (is_both_const(left, right))
    {
        left->const_expr.value = value_sub(left->const_expr.value, right->const_expr.value);
        if (left->const_expr.value.type == VALUE_TYPE_ERROR)
        {
            FATAL_ERROR("Value folding failed");
        }
        return left;
    }
    return binexpr(BINOP_SUB, left, right);
}

Expr *sema_expr_binary(TokenType type, Expr *left_side, Expr *right_side)
{
    BinOp op = binop_from_token(type);
    assert(op != BINOP_ERROR);
    switch (op)
    {
        case BINOP_ERROR:
            UNREACHABLE
        case BINOP_ASSIGN:
            //sema_expr_assign(left_side, right_side);
            break;
        case BINOP_MULT:
            return sema_expr_mult(left_side, right_side);
        case BINOP_MULT_ASSIGN:break;
        case BINOP_ADD:
            return sema_expr_add(left_side, right_side);
        case BINOP_ADD_ASSIGN:break;
        case BINOP_SUB:
            return sema_expr_sub(left_side, right_side);
        case BINOP_SUB_ASSIGN:break;
        case BINOP_DIV:break;
        case BINOP_DIV_ASSIGN:break;
        case BINOP_MOD:break;
        case BINOP_MOD_ASSIGN:break;
        case BINOP_AND:break;
        case BINOP_AND_ASSIGN:break;
        case BINOP_OR:break;
        case BINOP_OR_ASSIGN:break;
        case BINOP_BIT_AND:break;
        case BINOP_BIT_AND_ASSIGN:break;
        case BINOP_BIT_OR:break;
        case BINOP_BIT_OR_ASSIGN:break;
        case BINOP_BIT_XOR:break;
        case BINOP_BIT_XOR_ASSIGN:break;
        case BINOP_NE:break;
        case BINOP_EQ:break;
        case BINOP_GE:break;
        case BINOP_GT:break;
        case BINOP_LE:break;
        case BINOP_LT:break;
        case BINOP_SHR:break;
        case BINOP_SHR_ASSIGN:break;
        case BINOP_SHL:break;
        case BINOP_SHL_ASSIGN:break;
        case BINOP_ELVIS:break;
    }
    return NULL;
}



static bool sema_analyse_variable(Decl *variable)
{
    if (!variable->is_unparsed) return true;
    if (variable->is_being_parsed)
    {
        sema_error_at(variable->span, "Circular dependence resolving '%s'", variable->name);
        return false;
    }
    variable->is_being_parsed = true;
    DEBUG_LOG("Analyse variable %s", variable->name);
    assert(variable->type_id == DECL_VAR);
    assert(variable->var.kind == VARDECL_GLOBAL);
    QualifiedType type = parse_type_in_range(&variable->var.unparsed.type_range, variable->is_public);
    if (IS_INVALID(type)) return false;
    Expr *expr = NULL;
    if (variable->var.unparsed.init_range.length)
    {
        expr = parse_expression_block(&variable->var.unparsed.init_range);
        if (!expr) return false;
        if (!perform_assignment_conversions(type, expr)) return false;
    }
    if (IS_CONST(type) && !expr)
    {
        sema_error_at(variable->span, "Constant variable '%s' must be initialized", variable->name);
        return false;
    }
    if (expr && expr->expr_id == EXPR_CONST)
    {
        printf("%s = ", variable->name);
        value_print(expr->const_expr.value);
        printf("\n");
    }
    variable->var.original_type = type;
    variable->type = (QualifiedType) { .type = type.type };
    variable->var.init_expr = expr;
    variable->is_unparsed = false;
    variable->is_being_parsed = false;
    return true;
}
static void sema_analyse_type(Decl *type)
{
    if (!type->is_unparsed) return;
    if (type->is_being_parsed)
    {
        sema_error_at(type->span, "Cyclic dependency on %s", type->name);
        return;
    }
    type->is_being_parsed = true;
    bool success = false;
    switch (type->type_id)
    {
        case DECL_ALIAS_TYPE:
            success = sema_analyse_alias(type);
            break;
        case DECL_STRUCT_TYPE:
            success = sema_analyse_struct(type);
            break;
        case DECL_UNION_TYPE:
            success = sema_analyse_union(type);
            break;
        case DECL_ENUM_TYPE:
            success = sema_analyse_enum_type(type);
            break;
        case DECL_FUNC_TYPE:
            //sema_analyse_func(type);
            break;
        default:
            UNREACHABLE
    }
    type->is_being_parsed = false;
    type->is_unparsed = false;
    if (!success)
    {
        TODO
        // Remove
    }
}

static void resolve_types(void)
{
    Vector *types = context->parser->types;
    for (unsigned i = 0 ; i < types->size; i++)
    {
        sema_analyse_type(types->entries[i]);
    }
}

static void resolve_variables(void)
{
    Vector *variables = context->parser->variables;
    for (unsigned i = 0 ; i < variables->size; i++)
    {
        sema_analyse_variable(variables->entries[i]);
    }
}

void sema_analyse(void)
{
    resolve_types();
    resolve_variables();
}
