#lang plai-typed
; Core da linguagem
(define-type langC
  [numC (n : number)]
  [idC  (s : symbol)] 
  [appC (fun : langC) (arg : langC)]
  [plusC (l : langC) (r : langC)]
  [multC (l : langC) (r : langC)]
  [divC (l : langC) (r : langC)]
  [modC (l : langC) (r : langC)]
  [ifC   (condição : langC) (sim : langC) (não : langC)]
  [lamC (arg : symbol) (body : langC)] ; nomes não são mais necessários
  )

; Definição de função com nome, um argumento e corpo
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : langC)]
  )

; Corpo da linguagem "açucarado"
(define-type langS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [appS    (fun : langS) (arg : langS)] 
  [plusS   (l : langS) (r : langS)]
  [bminusS (l : langS) (r : langS)]
  [uminusS (e : langS)]
  [multS   (l : langS) (r : langS)]
  [modS (l : langS) (r : langS)]
  [divS (l : langS) (r : langS)]
  [ifS     (c : langS) (s : langS) (n : langS)]
  [lamS    (arg : symbol) (body : langS)] ; muda de acordo
  )

; Desaçucarador
(define (desugar [as : langS]) : langC  
  (type-case langS as
    [numS    (n)   (numC n)]
    [idS     (s) (idC s)]
    [lamS     (a b)  (lamC a (desugar b))] ; idem
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [divS (l r) (divC (desugar l) (desugar r))]
    [modS (l r) (modC (desugar l) (desugar r))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))

(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : langC) (env : Env)])

(define-type Binding
      [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)        ; ente pronunciar "mt" em inglês e compare com "empty"
(define extend-env cons)     ; sorte, cons faz exatamente o que queremos para estender o env

(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

; Substituição de "valor" no símbolo "isso" dentro do corpo da função 
(define (num/ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
            (cond [(zero? (numV-n r)) (error 'num/ "Divisão por zero")] [else (numV (/ (numV-n l) (numV-n r)))])
        ]
        [else 
            (error 'num/ "Um dos argumentos não é número")]))

(define (num% [l : Value] [r : Value]) : Value
     (cond
        [(and (numV? l) (numV? r))
            (numV (modulo (numV-n l) (numV-n r)))]
        [else 
            (error 'mod "Um dos argumentos não é número")]))

; Interpretador da linguagem
(define (interp [a : langC] [env : Env]) : Value
  (type-case langC a
    [numC (n) (numV n)]
    [appC (f a)
          (local ([define f-value (interp f env)]) ; f-value descreve melhor a ideia
            (interp (closV-body f-value)
                    (extend-env 
                        (bind (closV-arg f-value) (interp a env))
                        (closV-env f-value) ; não mais mt-env
                    )))]
    [lamC (a b) (closV a b env)] ; definição de função captura o environment
    [idC (n) (lookup n env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [divC (l r) (num/ (interp l env) (interp r env))]
    [modC (l r) (num% (interp l env) (interp r env))]
    [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]))
    

; lookup também muda o tipo de retorno
(define (lookup [for : symbol] [env : Env]) : Value
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

; Parser da linguagem
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
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(/) (divS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(%) (modS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; Biblioteca com algumas definições de função
;;; (define biblioteca (list 
;;;                     [fdC 'dobro 'x (plusC (idC 'x) (idC 'x))]
;;;                     [fdC 'quadrado 'y (multC (idC 'y) (idC 'y))]
;;;                     [fdC 'fatorial 'n (ifC  (idC 'n) 
;;; 						 (multC (appC 'fatorial (plusC (idC 'n) (numC -1))) 
;;; 								(idC 'n))
;;; 						 (numC 1))]
;;;                     [fdC 'narciso  'narciso (multC (idC 'narciso) (numC 1000))]
;;;                     [fdC 'metade 'x (divC (idC 'x) (numC 2))]
;;;                     [fdC 'ehPar 'x (ifC (modC (idC 'x) (numC 2)) (numC 0) (numC 1))]
                    
                    
;;;                     ))

; Leitura de input