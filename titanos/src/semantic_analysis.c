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

typedef struct _Analyser {
    //TypeMap typeMap;
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

static Ast *resolve_type(Token *token)
{
    Ast *identifier = table_get(&symbols, token->start, token->length);
    if (!identifier)
    {
        error_at(token, "Unknown type");
        return NULL;
    }
    return identifier;
}

static bool register_type(Ast *type)
{
    switch (type->type)
    {
        case AST_STRUCT_TYPE:
            return register_type_named(&type->struct_type.name, type);
        case AST_FUNC_TYPE:
            return register_type_named(&type->func_type.name, type);
        case AST_ALIAS_TYPE:
            return register_type_named(&type->alias_type.alias, type);
        case AST_ENUM_TYPE:
            return register_type_named(&type->enum_type.name, type);
        default:
            FATAL_ERRORF("Unhandled type");
    }
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
    Ast *old = table_set(&symbols, function->func_decl.name->span.start, function->func_decl.name->span.length, function);

    if (old)
    {
        error_at(&function->func_decl.name->span, "Redefinition of symbol");
        error_at(&old->span, "Previous definition was here");
        return false;
    }
    return true;
}


static void register_builtin(char *name, BuiltinFamily family, unsigned bits)
{
    Token token = {
            .length = (uint32_t)strlen(name),
            .type = TOKEN_IDENTIFIER,
            .start = name,
            .line = 0 };
    Ast *builtin = new_ast_with_span(AST_BUILTIN_TYPE, &token);
    builtin->builtin_type.type = family;
    builtin->builtin_type.bits = bits;
    table_set(&symbols, name, token.length, builtin);
}
static void register_built_in_types()
{
    register_builtin("u8", BUILTIN_UINT, 8);
    register_builtin("u16", BUILTIN_UINT, 16);
    register_builtin("u32", BUILTIN_UINT, 32);
    register_builtin("u64", BUILTIN_UINT, 64);
    register_builtin("i8", BUILTIN_INT, 8);
    register_builtin("i16", BUILTIN_INT, 16);
    register_builtin("i32", BUILTIN_INT, 32);
    register_builtin("i64", BUILTIN_INT, 64);
    register_builtin("f32", BUILTIN_FLOAT, 32);
    register_builtin("f64", BUILTIN_FLOAT, 64);
    register_builtin("bool", BUILTIN_UINT, 1);
}

bool analyse(void)
{
    table_init(&symbols, 1024 * 16);
    register_built_in_types();
    bool success = true;
    for (int i = 0; i < parser.types->size; i++)
    {
        success = success & register_type(parser.types->entries[i]);
    }
    for (int i = 0; i < parser.functions->size; i++)
    {
        success = success & register_function(parser.functions->entries[i]);
    }

    // analyse_module(ast->source.module);
    printf("Done %d registered %d", success, symbols.count);
    return success;
}
