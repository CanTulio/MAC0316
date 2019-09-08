D [0-9]
E [Ee][+-]?{D}+
%{
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "mcalc.tab.h"
%}

%%
if { return IF; }

[a-zA-Z]+    { 
    yylval.fun = malloc(strlen(yytext)+1);
    strcpy(yylval.fun, yytext);
    return FUN; }  /* Regex para detectar chamadas de função */

{D}*[.]?{D}+{E}?	{ yylval.val = malloc(strlen(yytext)+1);
                      strcpy(yylval.val, yytext);
                      return NUM; } /* Detecta números */
[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN;  }
[)]		{ return CLOSE; }
[:]     { return COL;   }
[?]     { return QTN;   }
[+]		{ return ADD;   }
[-]		{ return SUB;   }
[*]		{ return MUL;   }
[/]		{ return DIV;   }
[%]     { return MOD;   }

.	{  fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%
