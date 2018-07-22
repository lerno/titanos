%{
#include <stdio.h>
int regs[26];
int base;
%}
%start source
%token DIGIT LETTER
%left '|'
%token LOWERCASEIDENT
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
source: MODULE LOWERCASEIDENT EOS imports_and_rest | imports_and_rest ;

imports_and_rest: imports toplevel | toplevel;

imports: import | import imports;


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
