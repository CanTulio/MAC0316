tudo: lexigram lang

lexigram: lexigram.tab.o lex.yy.o main.o 
	gcc -o $@ $^  -lfl

lexigram.tab.o: lexigram.y
	bison -d lexigram.y
	gcc -c lexigram.tab.c

lex.yy.o: lexigram.l
	flex lexigram.l
	gcc -c lex.yy.c

lang: lang.rkt
	raco exe $<

clean:
	rm -f *.o lex.yy.c lexigram.tab.c lexigram.tab.h lang *~
