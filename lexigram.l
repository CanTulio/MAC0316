D [0-9]
E [Ee][+-]?{D}+
%{
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "lexigram.tab.h"
%}

%%
if { return IF; }



{D}*[.]?{D}+{E}?	{ yylval.val = malloc(strlen(yytext)+1);
                      strcpy(yylval.val, yytext);
                      return NUM; } /* Detecta números */
[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[{]     { return OPC;   }
[}]     { return CLC;   }
[,]     { return COM;   }
[(]		{ return OPEN;  }
[)]		{ return CLOSE; }
[:]     { return COL;   }
[?]     { return QTN;   }
[+]		{ return ADD;   }
[-]		{ return SUB;   }
[*]		{ return MUL;   }
[/]		{ return DIV;   }
[%]     { return MOD;   }
=       { return ATR;   } /*Atribuição*/
let     { return LET;   }

=>      { return ARROW; }
[a-zA-Z]+    { 
    yylval.arg = malloc(strlen(yytext)+1);
    strcpy(yylval.arg, yytext);
    return ID; }  /* Regex para detectar chamadas de função */

.	{  fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%
