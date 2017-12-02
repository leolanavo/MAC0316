#lang plai-typed

#|
 | Adding boxes, sequences with 2 expressions, variables and lists
 |#

(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [boxC    (arg : ExprC)]; Create a box
  [unboxC  (arg : ExprC)]; Unpacks the value inside a box
  [setboxC (b : ExprC) (v : ExprC)]; Replaces the value inside a box
  [seqC    (b1 : ExprC) (b2 : ExprC)]; Executes b1 then b2
  [consC   (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC    (pair : ExprC)]; Gets 1st element of a pair
  [cdrC    (pair : ExprC)]; Gets 2nd element of a pair
  [setC    (var : symbol) (arg : ExprC)]; Attribution of variable
  [equalC  (l : ExprC) (r : ExprC)]
  [letrecC (s : symbol) (body : ExprC) (cal : ExprC)]
  )

(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [boxS    (a : ExprS)]
  [unboxS  (a : ExprS)]
  [setboxS (b : ExprS) (v : ExprS)]
  [seqS    (b1 : ExprS) (b2 : ExprS)]
  [consS   (car : ExprS) (cdr : ExprS)]
  [carS    (pair : ExprS)]
  [cdrS    (pair : ExprS)]
  [setS    (var : symbol) (arg : ExprS)]
  [equalS  (l : ExprS) (r : ExprS)]
  [letS    (s : symbol) (body : ExprS) (val : ExprS)]
  [letrecS (s : symbol) (body : ExprS) (cal : ExprS)]
  )

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [boxS    (a)        (boxC   (desugar a))]
    [unboxS  (a)        (unboxC (desugar a))]
    [setboxS (b v)      (setboxC (desugar b) (desugar v))]
    [seqS    (b1 b2)    (seqC (desugar b1) (desugar b2))]
    [consS   (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS    (c)        (carC (desugar c))]
    [cdrS    (c)        (cdrC (desugar c))]
    [setS    (var expr) (setC var (desugar expr))]
    [equalS  (l r)      (equalC (desugar l) (desugar r))]
    [letS    (s body val) (appC (lamC s (desugar body)) (desugar val))]
    [letrecS (s body cal) (letrecC s (desugar body) (desugar cal))]
    ))

; We need storage and location
(define-type-alias Location number)

; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV  (l : Location)] ; Points to the location
  [consV (car : Location) (cdr : Location)]
  [suspV (body : ExprC) (env : Env)]
)

; Bindings associate symbol with location
(define-type Binding
        [bind (name : symbol) (val : Location)])

; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store
(define-type Storage
      [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))

(define mt-store empty)
(define override-store cons)

(define dummy (closV 'x (idC 'x) mt-env))

; lookup changes its return type
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

(define (fetchSusp [l : Location] [sto : Store]): Result
  (let ([x (fetch l sto)])
    (if (suspV? x)
      (type-case Result (interp (suspV-body x) (suspV-env x) sto)
        [v*s (v-x s-x)
          (v*s v-x s-x)])
      (v*s x sto))))

; fetch is equivalent to a lookup for the store
(define (fetch [l : Location] [sto : Store]) : Value
  (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l   (cell-location (first sto)))   ; achou!
                                 (cell-val (first sto))]
                  [else (fetch l (rest sto))])]))      ; vê no resto

; Returns the next location available
(define new-loc
   (let ( [ n (box 0)])
        (lambda ()
           (begin
              (set-box! n (+ 1 (unbox n)))
              (unbox n)))))


; Auxiliar operators
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



 ; New return type for our interpreter, Env and Store
(define-type Result
      [v*s (v : Value) (s : Store)])


; interp receives and returns Store
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [idC  (n) (fetchSusp (lookup n env) sto)]

    [lamC (a b) (v*s (closV a b env) sto)]


    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; result and store retorned by b1
                          (interp b2 env s-b1)])]

    ; application of function
    [appC (f a)
      (type-case Result (interp f env sto) ; find the function
         [v*s (v-f s-f)
              (type-case Result (v*s (suspV a env) s-f) ; argument with sto changed by the function
                 [v*s (v-a s-a)
                        (let* ([where     (new-loc)]
                               [body      (closV-body v-f)]
                               [new-env   (extend-env (bind (closV-arg v-f) where) (closV-env v-f))]
                               [new-store (override-store (cell where v-a) s-a)])
                            (interp body new-env new-store)
                           )])])]

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

    ; ifC serializes
    [ifC (c s n) (type-case Result (interp c env sto)
                   [v*s (v-c- s-c)
                        (if (zero? (numV-n (v*s-v (interp c env sto))))
                            (interp n env s-c)
                            (interp s env s-c))])]

    ; Creates a box: it needs a value and a new location
    [boxC (a)
          (type-case Result (interp a env sto)
            [v*s (v-a s-a)
                 (let ([where (new-loc)])
                   (v*s (boxV where)
                        (override-store (cell where v-a) s-a)))])]

    ; Get the value from a box
    [unboxC (a) (type-case Result (interp a env sto)
                  [v*s (v-a s-a)
                       (v*s
                        (fetch (boxV-l v-a) s-a) ; value
                        s-a                      ; store
                        )])]

    ; Replace the value inside a box
    [setboxC (b v) (type-case Result (interp b env sto); Result is a pair
                     [v*s (v-b s-b)
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v
                                      (override-store
                                       (cell (boxV-l v-b)
                                             v-v)
                                       s-v))])])]
    ; Attribution of variables
    [setC (var val) (type-case Result (interp val env sto)
                      [v*s (v-val s-val)
                           (let ([where (lookup var env)]) ; finds the variable
                             (v*s v-val
                                  (override-store ; updates
                                   (cell where v-val) s-val)))])]

    ; Working with lists
    [consC (b1 b2) (type-case Result (v*s (suspV b1 env) sto)
                     [v*s (v-b1 s-b1)
                          (type-case Result (v*s (suspV b2 env) s-b1)
                            [v*s (v-b2 s-b2)
                                 (let ((where-b1 (new-loc)) (where-b2 (new-loc)))
                                   (v*s (consV where-b1 where-b2)
                                        (override-store (cell where-b2 v-b2)
                                                        (override-store (cell where-b1 v-b1)
                                                                        s-b2))))])])]

    [carC (c) (type-case Result (interp c env sto)
                [v*s (v-c s-c)
                     (fetchSusp (consV-car v-c) s-c)])]

    [cdrC (c) (type-case Result (interp c env sto)
                [v*s (v-c s-c)
                     (fetchSusp (consV-cdr v-c) s-c)])]

    [equalC (l r) (type-case Result (interp l env sto)
                    [v*s (v-l s-l)
                      (type-case Result (interp r env s-l)
                        [v*s (v-r s-r)
                          (equalV v-l v-r s-r)])])]

    [letrecC (s body cal)
            (let* ([x (new-loc)]
                   [env-aux  (extend-env (bind s x) env)]
                   [sto-aux  (override-store (cell x dummy) sto)]
                   [f-return (interp body env-aux sto-aux)]
                   [sto-fin  (override-store (cell x (v*s-v f-return)) sto)])
              (interp cal env-aux sto-fin))]

    ))

(define (equalV_true [sto : Store]) : Result
  (v*s (numV 1) sto))

(define (equalV_false [sto : Store]) : Result
  (v*s (numV 0) sto))

(define (equalV [l : Value] [r : Value] [sto : Store]) : Result
  (cond
    [(and (numV? l) (numV? r))
      (if (equal? l r) (equalV_true sto) (equalV_false sto))]

    [(and (closV? l) (closV? r))
     (if (equal? l r) (equalV_true sto) (equalV_false sto))]

    [(and (consV? l) (consV? r))
      (if (and
          (equal?
            (v*s-v (equalV (v*s-v (fetchSusp (consV-car l) sto)) (v*s-v (fetchSusp (consV-car r) sto)) sto))
            (numV 1))
          (equal?
            (v*s-v (equalV (v*s-v (fetchSusp (consV-cdr l) sto)) (v*s-v (fetchSusp (consV-cdr r) sto)) sto))
            (numV 1)))
        (equalV_true sto)
        (equalV_false sto))]

     [else (equalV_false sto)]))

; Parser with funny instructions for boxes
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(-#) (boxS (parse (second sl)))]
         [(>#) (unboxS (parse (second sl)))]
         [(!#) (setboxS (parse (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]

         ; Equal
         [(equal?) (equalS (parse(second sl)) (parse(third sl)))]

         ;Lets
         [(let) (letS (s-exp->symbol(first(s-exp->list(first(s-exp->list(second sl))))))
                      (parse(third sl))
                      (parse(second(s-exp->list(first(s-exp->list(second sl)))))))]

         [(let*) (letS (s-exp->symbol(first(s-exp->list(first(s-exp->list(second sl))))))
                       (letS (s-exp->symbol(first(s-exp->list(second(s-exp->list(second sl))))))
                             (parse(third sl))
                             (parse(second(s-exp->list(second(s-exp->list(second sl)))))))
                       (parse(second(s-exp->list(first(s-exp->list(second sl)))))))]

         [(letrec) (letrecS  (s-exp->symbol(first(s-exp->list(first(s-exp->list(second sl))))))
                             (parse(second(s-exp->list(first(s-exp->list(second sl))))))
                             (parse(third sl)))]

         ;SuspV
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env mt-store))


; Examples

;(interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))

;(interpS '(call (lambda x (seq (:= x (+ x 5))x)) 8))

;(interpS '(seq (!# (-# 2) 32) (># (-# 2) (+ (># (-# 2)) 10))))

;(interpS '(call (lambda f (call f (-# 32))) (lambda x (seq (!# x (+ (># x) 10)) (># x)))))


; Tests

(test (v*s-v (interpS '(equal? 1 1))) (numV 1))
(test (v*s-v (interpS '(equal? (lambda x (+ x 5)) (lambda x (+ x 5))))) (numV 1))
(test (v*s-v (interpS '(equal? (cons 15 16) (cons 15 16)))) (numV 1))
(test (v*s-v (interpS '(equal? (cons 14 (cons 15 16)) (cons 14 (cons 15 16))))) (numV 1))

(test (v*s-v (interpS '(let ((x 6)) (+ x 5)))) (numV 11))
(test (v*s-v (interpS '(let* ((x 5) (y 6)) (+ x y)))) (numV 11))
(test (v*s-v (interpS '(letrec ([fac (lambda n (if n (* n (call fac (- n 1))) 1))]) (call fac 5)))) (numV 120))

(test (v*s-v (interpS '(car (cons (+ 1 15) (+ 3 16))))) (numV 16))
(test (v*s-v (interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))) (numV 25))
(test (v*s-v (interpS '(+ 10 (call (lambda x (cdr x)) (cons 15 16))))) (numV 26))
(test (v*s-v (interp (carC (consC (numC 10) (numC 20))) mt-env mt-store)) (numV 10))
(test (v*s-v (interpS '(letrec ([ints-from (lambda n (cons n (call ints-from (+ 1 n))))])
   (let ([ints (call ints-from 0)])
     (car (cdr (cdr (cdr (cdr (cdr ints))))))
 )))) (numV 5))
