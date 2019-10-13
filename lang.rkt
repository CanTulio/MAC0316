#lang plai-typed
; Core da linguagem
(define-type langC
  [numC (n : number)]
  [varC  (s : symbol)]
  [appC (fun : langC) (arg : langC)]
  [plusC (l : langC) (r : langC)]
  [multC (l : langC) (r : langC)]
  [divC (l : langC) (r : langC)]
  [modC (l : langC) (r : langC)]
  [ifC   (condição : langC) (sim : langC) (não : langC)]
  [lamC (arg : symbol) (body : langC)]
  [setC (var : symbol) (arg : langC)] 
  [seqC (b1 : langC) (b2 : langC)] 
  )

; Corpo da linguagem "açucarado"
(define-type langS
  [numS    (n : number)]
  [varS    (s : symbol)]
  [appS    (fun : langS) (arg : langS)] 
  [plusS   (l : langS) (r : langS)]
  [bminusS (l : langS) (r : langS)]
  [uminusS (e : langS)]
  [multS   (l : langS) (r : langS)]
  [modS (l : langS) (r : langS)]
  [divS (l : langS) (r : langS)]
  [ifS     (c : langS) (s : langS) (n : langS)]
  [lamS    (arg : symbol) (body : langS)] ; muda de acordo
  [setS    (var : symbol) (arg : langS)]
  [seqS    (b1 : langS) (b2 : langS)]
  )

; Desaçucarador
(define (desugar [as : langS]) : langC  
  (type-case langS as
    [numS    (n)   (numC n)]
    [varS     (s) (varC s)]
    [lamS     (a b)  (lamC a (desugar b))] ; idem
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [divS (l r) (divC (desugar l) (desugar r))]
    [modS (l r) (modC (desugar l) (desugar r))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [setS    (s v)   (setC s (desugar v))]
    [seqS    (b1 b2) (seqC (desugar b1) (desugar b2))]
    ))

; Precisamos de Storage e Locations
(define-type-alias Location number)

(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : langC) (env : Env)])

(define-type Binding
      [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)        ; ente pronunciar "mt" em inglês e compare com "empty"
(define extend-env cons)     ; sorte, cons faz exatamente o que queremos para estender o env

(define-type Storage
      [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

; lookup também muda o tipo de retorno
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

(define (fetch [l : Location] [sto : Store]) : Value
       (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l   (cell-location (first sto)))   ; achou!
                                 (cell-val (first sto))]
                  [else (fetch l (rest sto))])]))        ; vê no resto

(define new-loc
   (let ( [ n (box 0)])
        (lambda () 
           (begin
              (set-box! n (+ 1 (unbox n)))
              (unbox n)))))

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

(define-type Result
      [v*s (v : Value) (s : Store)])

; Interpretador da linguagem
(define (interp [a : langC] [env : Env] [sto : Store]) : Result
  (type-case langC a
    [numC (n) (v*s (numV n) sto)] 
    [varC (n)  (v*s (fetch (lookup n env) sto) sto)]  ; busca em cascata, env e em seguida no sto
    [lamC (a b) (v*s (closV a b env) sto)]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; resultado e store retornado por b1
                          (interp b2 env s-b1)])]
    ; aplicação de função
    [appC (f a)
      (type-case Result (interp f env sto) ; acha a função
         [v*s (v-f s-f)
              (type-case Result (interp a env s-f) ; argumento com sto modificado pela função
                 [v*s (v-a s-a)
                      (let ([onde (new-loc)]) ; aloca posição para o valor do argumento
                           (interp (closV-body v-f) ; corpo
                                   (extend-env (bind (closV-arg v-f) onde) ; com novo argumento
                                       (closV-env v-f))
                                   (override-store (cell onde v-a) s-a))) ; com novo valor
                  ])])]
    [plusC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    [divC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num/ v-l v-r) s-r)])])]
    [modC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num% v-l v-r) s-r)])])]
    ; ifC já serializa
    [ifC (c s n) (if (zero? (numV-n (v*s-v (interp c env sto)))) (interp n env sto) (interp s env sto))]
    

    [setC (var val) (type-case Result (interp val env sto)
                     [v*s (v-val s-val)
                          (let ([onde (lookup var env)]) ; acha a variável
                            (v*s v-val
                                 (override-store ; atualiza
                                  (cell onde v-val) s-val)))])]
    ))

; Parser da linguagem
(define (parse [s : s-expression]) : langS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (varS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(/) (divS (parse (second sl)) (parse (third sl)))]
         [(%) (modS (parse (second sl)) (parse (third sl)))]
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; Facilitador
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env mt-store))

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

(test (interpS '(/ 6 2)) (v*s (numV 3) mt-store))

(test (interpS '(% 5 2)) (v*s (numV 1) mt-store))

(test (v*s-v (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env mt-store))
      (numV 15))

(interp (desugar (parse '(+ 10 (call (func x (+ x x)) 16)))) mt-env mt-store)

(interp (desugar (parse '(call (func x (seq (:= x (+ x 10)) x)) 32))) mt-env mt-store)