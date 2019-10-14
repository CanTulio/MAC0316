%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *oper(char* op, char *l, char *r) {
	/* Função que recebe dois operandos l r e uma operação e retorna uma 
	string na forma (op l r) */
	char *res = malloc(strlen(l)+strlen(r)+strlen(op)+5);
	sprintf(res, "(%s %s %s)", op, l, r);
	return res;
}

char *lamPrint(char *arg, char *body) {
	/* Função que recebe uma função name com parametros "param" e retorna uma 
	string na forma (call name param) */

	char *res = malloc(strlen("lamC") + strlen(arg) + strlen(body) + 6);
	sprintf(res, "(%s %s %s)", "func", arg, body);
	return res;
}

char *callPrint(char *param, char *body, char *arg) {
	/* Função que recebe uma função name com parametros "param" e retorna uma 
	string na forma (call name param) */

	char *func = lamPrint(param, body);
	char *res = malloc(strlen("call") + strlen(func) + 6);
	sprintf(res, "(%s %s %s)", "call", func, arg);
	return res;
}

char *ifPrint(char *cond, char *t, char *f) {
	/* Função que recebe uma condição True (t), uma condição False(f) e retorna
	uma string na forma (if (cond) (t) (f)) */
	char *res = malloc(strlen(cond) + strlen(t) + strlen(f) + 8);
	sprintf(res, "(if %s %s %s)", cond, t, f);
	return res;
}
char *dup(char *orig) {
	/* Função que devolve orig*/
	char *res = malloc(strlen(orig)+1);
	strcpy(res,orig);
	return res;
}
int yylex();
void yyerror(char *);
%}

%union {
	char *val;
	char *arg;
}

%token	<val> NUM
%token  ADD SUB MUL DIV MOD IF COL QTN PRINT OPEN CLOSE ARROW ID LET ATR OPC CLC COM
%type	<val> exp
%type	<arg> ID 


%left ADD SUB
%left MUL DIV
%left NEG

/* Gramatica */
%%

input: 		
		| 		exp{ puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;

exp: 			NUM 		{ $$ = dup($1); }
		|		ID			{ $$ = dup($1); }
		|		IF OPEN exp CLOSE QTN exp COL exp{ $$ = ifPrint($3, $6, $8);}
		| 		exp ADD exp	{ $$ = oper("+", $1, $3);}
		| 		exp SUB exp	{ $$ = oper("-", $1, $3);}
		| 		exp MUL exp	{ $$ = oper("*", $1, $3);}
		|		exp DIV exp { $$ = oper("/", $1, $3);}
		|		exp MOD exp { $$ = oper("%", $1, $3);}
		|		OPEN OPEN ID CLOSE ARROW OPEN exp CLOSE CLOSE OPEN exp CLOSE { $$ = callPrint($3, $7, $11); }
		|		OPEN ID CLOSE ARROW OPEN exp CLOSE { $$ = lamPrint($2, $6); }
		| 		SUB exp %prec NEG  { $$ = oper("~", $2, "");} 
		| 		OPEN exp CLOSE	{ $$ = dup($2);}
		|		LET ID ATR exp	{ $$ = oper(":=", $2, $4);}
		|		OPC exp COM exp CLC { $$ = oper("seq", $2, $4);}
;
%%
void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
