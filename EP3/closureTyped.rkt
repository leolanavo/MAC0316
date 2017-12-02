#lang plai-typed

; Basic expressions
(define-type ExprC
             [numC    (n : number)]
             [idC     (s : symbol)]
             [plusC   (l : ExprC) (r : ExprC)]
             [multC   (l : ExprC) (r : ExprC)]
             [ifC     (c : ExprC) (y : ExprC) (n : ExprC)]
             [seqC    (e1 : ExprC) (e2 : ExprC)]
             [setC    (var : symbol) (arg : ExprC)]
             [letC    (name : symbol) (arg : ExprC) (body : ExprC)]
             [classC  (idF : symbol) (var : symbol) (m1 : ExprC) (m2 : ExprC)]
             [methodC (name : symbol) (arg : symbol) (body : ExprC)]
             [newC    (name : symbol) (arg : ExprC)]
             [sendC   (obj : symbol) (method : symbol) (arg : ExprC)]
             )


; Sugared expressions
(define-type ExprS
             [numS    (n : number)]
             [idS     (s : symbol)]
             [plusS   (l : ExprS) (r : ExprS)]
             [bminusS (l : ExprS) (r : ExprS)]
             [uminusS (e : ExprS)]
             [multS   (l : ExprS) (r : ExprS)]
             [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
             [seqS    (e1 : ExprS) (e2 : ExprS)]
             [setS    (var : symbol) (arg : ExprS)]
             [letS    (name : symbol) (arg : ExprS) (body : ExprS)]
             [classS  (idF : symbol) (var : symbol) (m1 : ExprS) (m2 : ExprS)]
             [methodS (name : symbol) (arg : symbol) (body : ExprS)]
             [newS    (name : symbol) (arg : ExprS)]
             [sendS   (obj : symbol) (method : symbol) (arg : ExprS)]
             )


; Removing the sugar
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
             [numS    (n)                (numC n)]
             [idS     (s)                (idC s)]
             [plusS   (l r)              (plusC (desugar l) (desugar r))]
             [multS   (l r)              (multC (desugar l) (desugar r))]
             [bminusS (l r)              (plusC (desugar l) (multC (numC -1) (desugar r)))]
             [uminusS (e)                (multC (numC -1) (desugar e))]
             [ifS     (c s n)            (ifC (desugar c) (desugar s) (desugar n))]
             [seqS    (e1 e2)            (seqC (desugar e1) (desugar e2))]
             [setS    (var expr)         (setC  var (desugar expr))]
             [letS    (n a b)            (letC n (desugar a) (desugar b))]
             [classS  (idF var m1 m2)    (classC idF var (desugar m1) (desugar m2))]
             [methodS (name arg body)    (methodC name arg (desugar body))]
             [newS    (name arg)         (newC name (desugar arg))]
             [sendS   (obj method arg)   (sendC obj method (desugar arg))]
             ))


; We need a new value for the box
(define-type Value
            [numV    (n : number)]
            [methodV (name : symbol) (arg : symbol) (body : ExprC)]
            [objectV (var : Binding) (class : symbol) (parent : Value)]
            [classV  (idF : symbol) (var : symbol) (m1 : Value) (m2 : Value)])

; Bindings associate symbol with location
(define-type Binding
             [bind (name : symbol) (val : (boxof Value))])

; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define ObjectClass (classV 'Object 'a (numV 1) (numV 2)))
(define Object (objectV (bind 'a (box (numV 0))) 'Object (numV 0)))

; Find the name of a variable
(define (lookup [for : symbol] [env : Env]) : (boxof Value)
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string for) " was not found"))] ; variable is undefined
    [else (cond
            [(symbol=? for (bind-name (first env)))   ; found it!
             (bind-val (first env))]
            [else (lookup for (rest env))])]))        ; check in the rest

; Auxiliary operators
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
      (numV (+ (numV-n l) (numV-n r)))]
    [else
      (error 'num+ "One of the arguments is not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
      (error 'num* "One of the arguments is not a number")]))

(define (newRec [s : symbol] [env : Env]) : Value
  (if (equal? s 'Object)
    Object
    (let ([classN (unbox (lookup s env))])
      (objectV (bind (classV-var classN) (box (numV 0))) s (newRec (classV-idF classN) env)))
    ))

(define-type MTH
      [me (mth : Value) (env : Env)])

(define (expand-env [env : Env] [obj : Value]) : Env
  (if (equal? (objectV-class obj) 'Object)
    env
    (expand-env (extend-env (objectV-var obj) env) (objectV-parent obj))))

(define (find-mth [obj : Value] [id : symbol] [env : Env]) : MTH
    (if (equal? (objectV-class obj) 'Object)
      (error 'find-mth (string-append "Class does not respond to the method " (symbol->string id)))
      (let ([classN (unbox (lookup (objectV-class obj) env))])
        (if (equal? (methodV-name (classV-m1 classN)) id)
          (me (classV-m1 classN) (expand-env env obj))
          (if (equal? (methodV-name (classV-m2 classN)) id)
            (me (classV-m2 classN) (expand-env env obj))
            (find-mth (objectV-parent obj) id env))))))

; Interpreter
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
             ; Numbers just evaluta to their equivalent Value
             [numC (n) (numV n)]

             ; IDs are retrieved from the Env and unboxed
             [idC (n) (unbox (lookup n env))]

             ; Application of function

             ; Sum two numbers using auxiliary function
             [plusC (l r) (num+ (interp l env) (interp r env))]

             ; Multiplies two numbers using auxiliary function
             [multC (l r) (num* (interp l env) (interp r env))]

             ; Conditional operator
             [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]

             ; Sequence of operations
             [seqC (b1 b2) (begin (interp b1 env) (interp b2 env))] ; No side effect between expressions!

             ; Attribution of variables
             [setC (var val) (let ([b (lookup var env)])
                               (begin (set-box! b (interp val env)) (unbox b)))]

             ; Declaration of variable
             [letC (name arg body)
                   (let* ([new-bind (bind name (box (interp arg env)))]
                          [new-env (extend-env new-bind env)])
                     (interp body new-env))]

             [classC (idF var m1 m2)
                   (classV idF var (interp m1 env) (interp m2 env))]

             [methodC (name arg body)
                   (methodV name arg body)]

             [newC (name val)
                   (let ([classN (unbox (lookup name env))])
                         (objectV (bind (classV-var classN) (box (interp val env))) name (newRec (classV-idF classN) env)))]

              [sendC (obj method arg)
                (let* ([objN (unbox (lookup obj env))]
                       [mthenv (find-mth objN method env)]
                       [mth (me-mth mthenv)]
                       [new-env (me-env mthenv)])
                         (interp (methodV-body mth)
                         (extend-env (bind 'self (box objN))
                                     (extend-env (bind (methodV-arg mth)
                                     (box (interp arg env))) new-env))))]
             ))


; Parser
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(let) (letS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (third sl)))]

         [(class)  (classS  (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)) (parse (list-ref sl 4)))]
         [(method) (methodS (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
         [(new)    (newS    (s-exp->symbol (second sl)) (parse (third sl)))]
         [(send)   (sendS   (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]

         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define obj-env (extend-env (bind 'Object (box ObjectClass)) mt-env))

(define (interpS [s : s-expression]) (interp (desugar (parse s)) obj-env))

(define wallet-env (extend-env (bind 'Wallet (box (interpS '(class Object money
                 (method credit amount (:= money (+ money amount)))
                 (method debit amount (:= money (- money amount))))))) obj-env))

(define (interpF [s : s-expression] [env : Env]) (interp (desugar (parse s)) env))

(define wlt-env (extend-env (bind 'WalletWithTax (box (interpS '(class Wallet tax
             (method credit amount (:= money (- (+ money amount) tax)))
             (method total dummy money))))) wallet-env))

(test
  (interpS '(let ([self 1])
              (let ([Self (class Object self
                                 (method self self (send self Self 2))
                                 (method Self self 3) )])
                (let ([self (new Self 4)])
                  (send self self 5))
                )))
  (numV 3))
