//
// Created by Christoffer Lernö on 2019-03-05.
//

#include <string.h>
#include "header_parsing.h"
#include "decl.h"
#include "diagnostics.h"
#include "parsing.h"
#include "semantic_analyser.h"
#include "error.h"
#include "ast_utils.h"


/**
 * module ::= MODULE IDENTIFIER EOS
 */
static inline void parse_module()
{
    // No module? Error?
    SourceRange span = tok.span;
    bool ok = true;
    if (!try_consume(TOKEN_MODULE))
    {
        error_at_current("Expected 'module' at the start of file");
        ok = false;
    }

    Token name = token_wrap("<Error>");
    // Expect the module name
    if (ok && !consume(TOKEN_IDENTIFIER, "Expected module name"))
    {
        recover_to(TOKEN_EOS);
        ok = false;
    }
    else
    {
        name = prev_tok;
    }

    if (ok && !consume(TOKEN_EOS, "Expected ';' after module name"))
    {
        // Just recover to the next EOS
        recover_to(TOKEN_EOS);
    }
    sema_add_module(span, name);
}

static inline void advance_and_verify(TokenType type)
{
    assert(tok.type == type);
    advance();
}

/**
 * import ::= IMPORT IDENTIFIER EOS
 *          | IMPORT IDENTIFIER AS IDENTIFIER EOS
 *          | IMPORT IDENTIFIER LOCAL EOS
 */
static inline void parse_import()
{
    advance_and_verify(TOKEN_IMPORT);

    SourceRange start = prev_tok.span;
    if (!consume(TOKEN_IDENTIFIER, "Expected an identifier"))
    {
        recover_to(TOKEN_EOS);
        return;
    }
    Token module_name = prev_tok;
    Token alias = {};
    ImportType import_type = IMPORT_TYPE_FULL;
    if (try_consume(TOKEN_AS))
    {
        if (!consume(TOKEN_IDENTIFIER, "Expected alias name"))
        {
            recover_to(TOKEN_EOS);
            return;
        }
        alias = prev_tok;
        import_type = IMPORT_TYPE_ALIAS;
    }
    else if (try_consume(TOKEN_LOCAL))
    {
        import_type = IMPORT_TYPE_LOCAL;
    }
    if (!consume(TOKEN_EOS, "Expected ';'"))
    {
        recover_to(TOKEN_EOS);
        return;
    }
    sema_add_import(start, module_name, alias, import_type);
}

/**
 * imports ::= import
 *           | imports import
 */
static inline void parse_imports()
{
    while (tok.type == TOKEN_IMPORT)
    {
        parse_import();
    }
}

static inline SourceRange consume_to(TokenType type)
{
    SourceRange range = tok.span;
    set_lexer_state(LEXER_STATE_IGNORE_KEYWORDS);
    while (tok.type != type && tok.type != TOKEN_EOF)
    {
        advance();
    }
    set_lexer_state(LEXER_STATE_NORMAL);
    range.length = prev_tok.span.loc.id - range.loc.id + prev_tok.span.length;
    return range;
}

static inline SourceRange consume_to_or(TokenType type1, TokenType type2)
{
    SourceRange range = tok.span;
    set_lexer_state(LEXER_STATE_IGNORE_KEYWORDS);
    while (tok.type != type1 && tok.type != type2 && tok.type != TOKEN_EOF)
    {
        advance();
    }
    set_lexer_state(LEXER_STATE_NORMAL);
    range.length = prev_tok.span.loc.id - range.loc.id + prev_tok.span.length;
    return range;
}

static inline void consume_to_eos_or_lbrace(void)
{
    set_lexer_state(LEXER_STATE_IGNORE_KEYWORDS);
    while (tok.type != TOKEN_EOS && tok.type != TOKEN_EOF && tok.type != TOKEN_LBRACE)
    {
        advance();
    }
    set_lexer_state(LEXER_STATE_NORMAL);
}


static inline SourceRange consume_to_end_brace(void)
{
    SourceRange range = tok.span;
    set_lexer_state(LEXER_STATE_IGNORE_KEYWORDS);
    while (tok.type != TOKEN_LBRACE && tok.type != TOKEN_EOF)
    {
        advance();
    }
    if (tok.type == TOKEN_EOF) goto exit;

    advance_and_verify(TOKEN_LBRACE);
    uint32_t indent = 1;
    while (1)
    {
        switch (tok.type)
        {
            case TOKEN_RBRACE:
                if (--indent == 0)
                {
                    goto exit;
                }
                break;
            case TOKEN_EOF:
                goto exit;
            case TOKEN_LBRACE:
                indent++;
                break;
            default:
                break;
        }
        advance();
    }
    exit:
    set_lexer_state(LEXER_STATE_NORMAL);
    range.length = tok.span.loc.id - range.loc.id + tok.span.length;
    return range;
}

static void recover_to_end_body_or_eos(void)
{
    consume_to_eos_or_lbrace();
    // We reached the end? exit!
    if (tok.type == TOKEN_EOF) return;

    // We found ';'? exit!
    if (tok.type == TOKEN_EOS)
    {
        recover_to(TOKEN_EOS);
        return;
    }

    // Otherwise we got '{' so parse to the end brace.
    consume_to_end_brace();
    recover_to(TOKEN_RBRACE);
}

/**
 * attributes ::= '@' (' attribute_list ')'
 *
 * attribute_list ::= attribute
 *                    attribute_list ',' attribute
 *
 * attribute ::= name
 *             | name '=' STRING
 */
static Vector *parse_attributes()
{
    advance_and_verify(TOKEN_AT);

    if (!consume(TOKEN_LPAREN, "Missing '(' after '@'")) return NULL;

    Vector *list = new_vector(4);

    while (true)
    {
        if (!consume(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
        Ast *attribute = new_ast_with_span(AST_ATTRIBUTE, prev_tok.span);
        attribute->attribute.name = prev_tok.string;
        Expr *arg = NULL;
        if (try_consume(TOKEN_EQ))
        {
            arg = parse_expression();
        }
        attribute->attribute.value = arg;
        vector_add(list, attribute);
        if (!try_consume(TOKEN_COMMA)) break;
    }
    if (!consume(TOKEN_RPAREN, "Expected ')'")) return NULL;
    return list;
}

/**
 * array_entry ::= IDENTIFIER '+=' init_value
 */
static void parse_array_entry()
{
    advance_and_verify(TOKEN_IDENTIFIER);

    Token name = prev_tok;

    advance_and_verify(TOKEN_PLUS_ASSIGN);

    SourceRange range = consume_to(TOKEN_EOS);

    sema_add_array_entry(name, range);

    consume(TOKEN_EOS, "Expected ';'");
}

/**
 * alias_type ::= definition EOS

 * @param public if the alias is public
 * @param span the initial span of the parse.
 * @return the created Decl* or NULL in case of error
 */
static void parse_alias_type(bool public, SourceLoc start)
{
    Token name = prev_tok;
    SourceRange unparsed_body = consume_to(TOKEN_EOS);
    if (!consume(TOKEN_EOS, "Expected ';'")) return;

    sema_add_alias(public, start, &name, unparsed_body);
}

/**
 * func_type ::= FUNC definition EOS
 * @param public if the function is public
 * @param span the initial span of the parse.
 */
static void parse_function_type(bool public, SourceLoc start)
{
    Token name = prev_tok;
    SourceRange unparsed_body = consume_to(TOKEN_EOS);
    if (!consume(TOKEN_EOS, "Expected ';'")) return;
    sema_add_func_type(public, start, &name, unparsed_body);
}


/**
 * enum_type ::= ENUM type attributes? '{' enum_body '}'
 *
 * @param public if the enum is public
 * @param span the initial range of the parse
 * @return void
 */
static void parse_enum_type(bool public, SourceLoc start)
{
    Token name = prev_tok;
    advance_and_verify(TOKEN_ENUM);

    SourceRange type_body = consume_to_or(TOKEN_AT, TOKEN_LBRACE);

    // We might reach the end of file.
    if (tok.type == TOKEN_EOF) return;

    Vector *attributes = NULL;

    if (tok.type == TOKEN_AT)
    {
        attributes = parse_attributes();
        // We need to handle recovery for attribute fails
        if (!attributes)
        {
            recover_to_end_body_or_eos();
            return;
        }
    }

    if (tok.type != TOKEN_LBRACE)
    {
        sema_error_at(tok.span, "Expected '{' here");
        // If we failed, find the first '{' and just walk to the end brace.
        consume_to_end_brace();
        reset_panic_mode();
        return;
    }

    // Get the body (including braces)
    SourceRange enum_body = consume_to_end_brace();

    if (!consume(TOKEN_RBRACE, "Expected '}'")) return;

    sema_add_enum(public, start, &name, attributes, type_body, enum_body);
}

/**
 * struct_or_union_type ::= UNION attributes? struct_block
 *                        | STRUCT attributes? struct_block
 * @param public if the function is public
 * @param start the first token belonging to the parse.
 */
static void parse_struct_or_union_type(bool public, SourceLoc start)
{
    bool is_struct = tok.type == TOKEN_STRUCT;

    Token name = prev_tok;
    advance();

    Vector *attributes = NULL;
    if (tok.type == TOKEN_AT)
    {
        attributes = parse_attributes();
        // We need to handle recovery for attribute fails
        if (!attributes)
        {
            if (tok.type == TOKEN_EOF) return;
            // Ok, so just walk to '{' then try to match it with '}'
            consume_to_end_brace();
            reset_panic_mode();
            return;
        }
    }

    if (tok.type != TOKEN_LBRACE)
    {
        sema_error_at(tok.span, "Expected '{' here");
        // If we failed, find the first '{' and just walk to the end brace.
        consume_to_end_brace();
        reset_panic_mode();
        return;
    }

    SourceRange unparsed_body = consume_to_end_brace();
    if (!consume(TOKEN_RBRACE, "Expected '}'")) return;

    if (is_struct)
    {
        sema_add_struct(public, start, &name, attributes, unparsed_body);
    }
    else
    {
        sema_add_union(public, start, &name, attributes, unparsed_body);
    }
}

/**
 * type_definition ::= TYPE IDENTIFIER type_body
 *                   | TYPE IDENTIFIER func_type
 *                   | TYPE IDENTIFIER STRUCT attributes? '{' struct_block '}'
 *                   | TYPE IDENTIFIER ENUM attributes? '{' enum_block '}'
 * @param public if the type is public or not.
 * @param start first part
 */
static void parse_type_definition(bool public, SourceLoc start)
{
    advance_and_verify(TOKEN_TYPE);

    if (!consume(TOKEN_IDENTIFIER, "Expected identifier after 'type'"))
    {
        recover_to(TOKEN_EOS);
        return;
    }

    switch (tok.type)
    {
        case TOKEN_FUNC:
            parse_function_type(public, start);
            return;
        case TOKEN_STRUCT:
        case TOKEN_UNION:
            parse_struct_or_union_type(public, start);
            return;
        case TOKEN_ENUM:
            parse_enum_type(public, start);
            return;
        default:
            parse_alias_type(public, start);
            return;
    }
}

/**
 * Parse type_expr [IDENTIFIER '.']? IDENTIFIER ['@' | EOS | <next_token>]
 *
 * @param next_token possible terminator
 * @param range range to return
 * @param name identifier name
 * @return true if parsing succeeded
 */
static inline bool parse_type_and_name(TokenType next_token, SourceRange *range, Token *name)
{
    range->loc = tok.span.loc;

    // First move through all type definitions + name
    while (tok.type != TOKEN_EOF && tok.type != TOKEN_AT && tok.type != next_token && tok.type != TOKEN_EOS)
    {
        advance();
    }

    // Did we reach the end??
    if (tok.type == TOKEN_EOF) return false;

    if (prev_tok.type != TOKEN_IDENTIFIER)
    {
        sema_error_at(prev_tok.span, "Expected variable name");
        // Recovery can be improved by differentiating between { and ;
        recover_to(TOKEN_EOS);
        return false;
    }

    *name = prev_tok;
    range->length = prev_tok.span.loc.id - range->loc.id;
    return true;
}

/**
 * func_definition ::= 'func' type_body (IDENTIFIER '.') IDENTIFIER '(' params ')' attributes? (compound_body | EOS)
 *
 * @param public is the function is public
 * @param start
 */
static void parse_func_definition(bool public, SourceLoc start)
{
    advance_and_verify(TOKEN_FUNC);

    Token name;
    SourceRange type_range;

    if (!parse_type_and_name(TOKEN_LPAREN, &type_range, &name))
    {
        recover_to_end_body_or_eos();
        return;
    }

    if (tok.type != TOKEN_LPAREN)
    {
        sema_error_at(tok.span, "Expected '('");
        recover_to_end_body_or_eos();
        return;
    }

    SourceRange param_range = consume_to(TOKEN_RPAREN);

    if (!consume(TOKEN_RPAREN, "Expected ')'"))
    {
        recover_to_end_body_or_eos();
        return;
    }
    Vector *attributes = NULL;

    if (tok.type == TOKEN_AT)
    {
        attributes = parse_attributes();
        if (!attributes)
        {
            recover_to_end_body_or_eos();
            return;
        }
    }

    // What happens after
    if (tok.type == TOKEN_EOS)
    {
        sema_add_func_definition(public, start, &name, attributes, type_range, param_range, param_range, false);
        return;
    }

    if (tok.type != TOKEN_LBRACE)
    {
        sema_error_at(tok.span, "Expected start of function body");
        return;
    }

    SourceRange unparsed_body = consume_to_end_brace();
    sema_add_func_definition(public, start, &name, attributes, type_range, param_range, unparsed_body, true);
}

/**
 * variable_definition ::= public[opt] type_specifier IDENTIFIER attributes[opt] variable_initialization
 *
 * variable_initialization ::= '=' expression EOS
 *                           | '=' init_values
 *                           | EOS
 *
 * @param public if it is public or not
 */
static void parse_var_definition(bool public, SourceLoc start)
{
    // TODO improve and check + handle local

    Vector *attributes = NULL;

    Token name;
    SourceRange type_range;

    if (!parse_type_and_name(TOKEN_EQ, &type_range, &name)) return;

    // Did we find attributes? If so parse them
    if (tok.type == TOKEN_AT)
    {
        attributes = parse_attributes();
        if (!attributes)
        {
            // Attribute parsing failed, so bail to EOS or EQ
            consume_to_or(TOKEN_EQ, TOKEN_EOS);
            if (try_consume(TOKEN_EQ))
            {
                // We might have a struct init
                if (tok.type == TOKEN_LBRACE)
                {
                    // Just walk to the end brace and recover
                    consume_to_end_brace();
                    recover_to(TOKEN_RBRACE);
                    return;
                }
            }
            // Recover to EOS
            recover_to(TOKEN_EOS);
            return;
        }
    }

    // Did we find '='?

    SourceRange init_body = { .length = 0 };
    if (try_consume(TOKEN_EQ))
    {
        // Check if struct init.
        if (tok.type == TOKEN_LBRACE)
        {
            // Go to end
            init_body = consume_to_end_brace();
            if (!consume(TOKEN_RBRACE, "Expected '}' here")) return;
        }
        else
        {
            init_body = consume_to(TOKEN_EOS);
            if (!consume(TOKEN_EOS, "Expected ';'")) return;
        }
    }
    else
    {
        if (!consume(TOKEN_EOS, "Expected ';'")) return;
    }
    sema_add_var_definition(public, start, &name, type_range, attributes, init_body);
}

/**
 *
 * @param public
 * @param start
 */
static void parse_macro_definition(bool public, SourceLoc start)
{
    TODO
}

/**
 * top_level ::= top_level
 *             | top_level top_level_item
 *
 * top_level_item ::= array_entry                     -> IDENTIFIER += init_value
 *                  | public[opt] type_definition     -> [public] type IDENTIFIER ...
 *                  | public[opt] function            -> [public] func ...
 *                  | public[opt] var_definition      -> [public] type_qualifier IDENTIFIER
 *                  | public[opt] macro               -> [public] macro ...
 */
static void parse_top_level()
{
    // IMPROVE: error recovery
    while (tok.type != TOKEN_EOF)
    {
        if (error_found()) return;
        if (tok.type == TOKEN_IDENTIFIER && lookahead(1).type == TOKEN_PLUS_ASSIGN)
        {
            parse_array_entry();
            continue;
        }
        SourceLoc start = tok.span.loc;
        bool public = try_consume(TOKEN_PUBLIC);
        switch (tok.type)
        {
            case TOKEN_TYPE:
                parse_type_definition(public, start);
                break;
            case TOKEN_IMPORT:
                error_at_current("Import found outside start of file.");
                recover_to(TOKEN_EOS);
                break;
            case TOKEN_MODULE:
                error_at_current("Module declaration needs to be at the start of the file.");
                recover_to(TOKEN_EOS);
                break;
            case TOKEN_FUNC:
                parse_func_definition(public, start);
                break;
            case TOKEN_MACRO:
                parse_macro_definition(public, start);
                break;
            default:
                parse_var_definition(public, start);
                break;
        }

    }
}

void parse_header()
{
    advance();
    parse_module();
    parse_imports();
    parse_top_level();
}