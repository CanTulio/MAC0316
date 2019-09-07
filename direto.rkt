#lang plai-typed

(define-type langC
  [numC (n : number)]
  [idC  (s : symbol)] 
  [appC (fun : symbol) (arg : langC)]
  [plusC (l : langC) (r : langC)]
  [multC (l : langC) (r : langC)]
  [divC (l : langC) (r : langC)]
  [modC (l : langC) (r : langC)]
  [ifC   (condição : langC) (sim : langC) (não : langC)])


(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : langC)]
  )


(define-type langS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [appS    (fun : symbol) (arg : langS)] 
  [plusS   (l : langS) (r : langS)]
  [bminusS (l : langS) (r : langS)]
  [uminusS (e : langS)]
  [multS   (l : langS) (r : langS)]
  [modS (l : langS) (r : langS)]
  [divS (l : langS) (r : langS)]
  [ifS     (c : langS) (s : langS) (n : langS)])


(define (desugar [as : langS]) : langC  
  (type-case langS as
    [numS    (n)   (numC n)]
    [idS     (s) (idC s)]
    [appS    (fun arg) (appC fun (desugar arg))]
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [divS (l r) (divC (desugar l) (desugar r))]
    [modS (l r) (modC (desugar l) (desugar r))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))

(define (subst [valor : langC] [isso : symbol] [em : langC]) : langC
  (type-case langC em
    [numC (n) em]   ; nada a substituir, repassa
    [idC (s) (cond  ; poderia ser 'if', mas existem coisas no futuro...
               [(symbol=? s isso) valor] ; símbolo, troque - PONTO FINAL
               [else em])] ; deixa quieto
    [appC  (f a) (appC f (subst valor isso a))] ; chamada de função 
		   	  	 	   	; arruma o argumento
    [plusC (l r) (plusC (subst valor isso l) (subst valor isso r))]
    [multC (l r) (multC (subst valor isso l) (subst valor isso r))]
    [ifC (c s n) (ifC   (subst valor isso c) 
			(subst valor isso s) (subst valor isso n))]
    [divC (l r) (divC (subst valor isso l) (subst valor isso r))]
    [modC (l r) (modC (subst valor isso l) (subst valor isso r))]
  ))


(define (interp [a : langC] [fds : (listof FunDefC)]) : number
  (type-case langC a
    [numC (n) n]
    [appC (f a) 
      (local ([define fd (get-fundef f fds)]) ; pega a definição em fd
        (interp (subst a                 ; interpreta o resultado de subst
                        (fdC-arg fd)
                        (fdC-body fd)
                        )
                fds))]
    [idC (_) (error 'interp "não deveria encontrar isso!")]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [divC (l r) (if (zero? (interp r fds))
                  (error 'interp "Invalid operation : division by 0")
                  (/ (interp l fds) (interp r fds)))]
    [modC (l r) (modulo (interp l fds) (interp r fds))]
    [ifC (c s n) (if (zero? (interp c fds)) (interp n fds) (interp s fds))]))



(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "referência para função não definida")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)] ; achou!
                   [else (get-fundef n (rest fds))] ; procura no resto
                   )]))

(define (parse [s : s-expression]) : langS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         ; para o parser precisamos um sinal negativo...
         [(~) (uminusS (parse (second sl)))]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(/) (divS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(%) (modS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define biblioteca (list 
                    [fdC 'dobro 'x (plusC (idC 'x) (idC 'x))]
                    [fdC 'quadrado 'y (multC (idC 'y) (idC 'y))]
                    [fdC 'fatorial 'n (ifC  (idC 'n) 
						 (multC (appC 'fatorial (plusC (idC 'n) (numC -1))) 
								(idC 'n))
						 (numC 1))]
                    [fdC 'narciso  'narciso (multC (idC 'narciso) (numC 1000))]
                    [fdC 'metade 'x (divC (idC 'x) (numC 2))]
                    [fdC 'ehPar 'x (ifC (modC (idC 'x) (numC 2)) (numC 0) (numC 1))]
                    
                    
                    ))

(interp (desugar (parse (read))) biblioteca)