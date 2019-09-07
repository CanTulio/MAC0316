/* Calculadora infixa */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
char *oper(char op, char *l, char *r) {
	char *res = malloc(strlen(l)+strlen(r)+6);
	sprintf(res, "(%c %s %s)", op, l, r);
	return res;
}
char *functionPrint(char *appS, char *name, char *param) {
	char *res = malloc(strlen(appS) + strlen(name) + strlen(param) + 6);
	sprintf(res, "(%s %s %s)", appS, name, param);
	return res;
}
char *dup(char *orig) {
	char *res = malloc(strlen(orig)+1);
	strcpy(res,orig);
	return res;
}
int yylex();
void yyerror(char *);
%}

%union {
	char *val;
	char *fun;
}

%token	<val> NUM
%token  ADD SUB MUL DIV MOD FUN PRINT OPEN CLOSE
%type	<val> exp 
%type	<fun> FUN 


%left ADD SUB
%left MUL DIV
%left NEG

/* Gramatica */
%%

input: 		
		| 		exp     { puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;

exp: 			NUM 		{ $$ = dup($1); }
		| 		exp ADD exp	{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp	{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp	{ $$ = oper('*', $1, $3);}
		|		exp DIV exp { $$ = oper('/', $1, $3);}
		|		exp MOD exp { $$ = oper('%', $1, $3);}
		|		FUN OPEN exp CLOSE { $$ = functionPrint("appS", $1, $3);} 
		| 		SUB exp %prec NEG  { $$ = oper('~', $2, "");} 
		| 		OPEN exp CLOSE	{ $$ = dup($2);}
		
;

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
