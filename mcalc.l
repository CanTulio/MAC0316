D [0-9]
E [Ee][+-]?{D}+
%{
#include <math.h>
#include "mcalc.tab.h"

%}

%%
{D}*[.]?{D}+{E}?	{ yylval.val = yytext; return NUM; }
[a-zA-Z]+    { yylval.fun = yytext; return FUN; }
[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN; }
[)]		{ return CLOSE; }
[+]		{ return ADD; }
[-]		{ return SUB; }
[*]		{ return MUL; }
[/]		{ return DIV; }
[%]     { return MOD; }

.	{  fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%

/* Local Variables: */
/* mode: c */
/* End: */
