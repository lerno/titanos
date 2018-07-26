%{
#include <stdio.h>
int regs[26];
int base;
%}
%start source
%token DIGIT LETTER
%left '|'
%token COLONCOLON
%token STRING
%token MODULE
%token IDENTIFIER
%token EOS
%token FALLTHROUGH
%token PROC
%token PROGRAM
%token SWITCH
%token STRUCT
%token CASE
%token ARROW
%token NUMBER
%token IMPORT
%token VAR
%token INC
%token DEC
%token RETURN
%token SHIFT_L
%token SHIFT_R
%token GE_OP
%token LE_OP
%token EQ_OP
%token NE_OP
%token IF
%token ELSE
%token FOR
%token VOID
%token UNDEFINED
%token WHILE
%token UNTIL
%token POW
%left '&'
%left '+' '-'
%left '*' '/' '%'
%left UMINUS  /*supplies precedence for unary minus */
%%                   /* beginning of rules section */


/** module foo.bar; **/
source: MODULE qualified_name EOS imports_and_rest | imports_and_rest ;

imports_and_rest: imports proc_or_programs | proc_or_programs;

imports: import | import imports;

qualified_name: IDENTIFIER | qualified_name COLONCOLON IDENTIFIER;

/* import foo.com; import a.*; */
import: IMPORT import_path EOS;

import_path: qualified_name | qualified_name COLONCOLON '*';

proc_or_programs:
	proc_or_program
	|
	proc_or_program proc_or_programs
	;

/* program foo { } */
/* proc foo(a : int, b: int) -> int {   } */

struct_content: IDENTIFIER ':' type EOS;
struct_contents: struct_content | struct_contents struct_content;

proc_or_program:
  STRUCT IDENTIFIER '{' struct_contents '}'
  |
	PROGRAM IDENTIFIER block
	|
	PROC IDENTIFIER '(' var_list ')' ARROW type block
	;

/* foo : int */
var_list_entry:
	IDENTIFIER ':' type;

/* foo : int, bar : string */
var_list:
	var_list_entry
	|
	var_list_entry ',' var_list
	;


pointer_type:
  IDENTIFIER
  |
  pointer_type '*'
  ;

complex_type:
  PROC '(' type_list ')' ARROW type
  |
  '(' type ')' '*'
  |
  pointer_type
  ;

type:
  complex_type
  |
  VOID
  ;

type_list:
	type
	|
	type_list ',' type
	;

block:
	'{' statements '}';

statements:
	statement
	|
	statements statement
	;

no_block_statement:
	variable_declaration EOS
  |
  RETURN expression EOS
  |
  expression EOS
  |
  EOS
  ;

statement:
  no_block_statement
  |
	if_statement
	|
	while_statement
	|
	until_statement
	|
	for_statement
	|
	switch_statement
	;

no_block_statements: no_block_statement | no_block_statements no_block_statement;



expressions:
  expression
  |
  expressions EOS expression
  ;

  variable_declaration:
  	IDENTIFIER ':' type '=' var_expression
    |
    IDENTIFIER ':' '=' var_expression
  	;

   variable_declarations:
   variable_declaration
   |
   variable_declarations EOS variable_declaration
;

switch_statement: SWITCH '(' expression ')' '{' cases '}';

cases: case | case cases;

case_start: CASE NUMBER ':' statements;

case: case_start | case_start FALLTHROUGH

for_head: '(' variable_declarations ':' expressions ':' expression ')';

for_statement:
	FOR for_head '{' statements '}'
	|
	FOR for_head ARROW no_block_statement EOS
  ;


in_block_expression:
	expression
	|
	variable_declarations ':' expression
	;

if_statement:
  IF '(' in_block_expression ')' ARROW no_block_statement EOS
	|
	IF '(' in_block_expression ')' '{' statements '}'
	|
	IF '(' in_block_expression ')' '{' statements '}' ELSE '{' statements '}'
	;

while_statement:
	WHILE '(' in_block_expression ')' no_block_statement EOS
	|
	WHILE '(' in_block_expression ')' '{' statements '}'
	;

until_statement:
	UNTIL '(' in_block_expression ')' no_block_statement EOS
	|
	UNTIL '(' in_block_expression ')' '{' statements '}'
	;




var_expression:
	UNDEFINED
	|
	expression
	;

expression:
	rel_expr
	;


primary_expr:
	IDENTIFIER
	|
	NUMBER
	|
	STRING
	|
	'(' expression ')'
	|
	'{' '%' no_block_statements '}'
	;

postfix_expr:
	primary_expr
	|
	postfix_expr '[' expression ']'
	|
	postfix_expr '(' expression_list ')'
	|
	postfix_expr '(' ')'
	|
	postfix_expr '.' IDENTIFIER
	|
	postfix_expr ARROW IDENTIFIER
	|
	postfix_expr INC
	|
	postfix_expr DEC
	;

expression_list:
	expression
	|
	expression ',' expression_list
	;

unary_ops: '!' | '*' | '&' | '~' ;

unary_expr:
	postfix_expr
	|
	unary_ops unary_expr
	|
	INC unary_expr
	|
	DEC unary_expr
	;

mult_ops: '*' | '/' | '%';

exp_expr: unary_expr | exp_expr POW unary_expr;

mult_expr:
	exp_expr
	|
	mult_expr mult_ops exp_expr
	;

shift_ops: SHIFT_R | SHIFT_L;

shift_expr: mult_expr | shift_expr shift_ops mult_expr;

add_op: '+' | '-';

add_expr: shift_expr | add_expr add_op shift_expr;

rel_ops: '>' | '<' | LE_OP | GE_OP | NE_OP | EQ_OP;

rel_expr: add_expr | rel_expr rel_ops add_expr;


%%
main()
{
 return(yyparse());
}
yyerror(s)
char *s;
{
  fprintf(stderr, "%s\n",s);
}
yywrap()
{
  return(1);
}
