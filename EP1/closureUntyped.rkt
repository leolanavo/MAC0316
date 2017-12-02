#lang plai

#|
 | Funções não têm mais nome, serão chamadas de lamC (em homenagem ao λ)
 |#

; Expressões básicas
(define-type ExprC
  [numC    (n  number?)]
  [idC     (s  symbol?)]
  [plusC   (l  ExprC?) (r  ExprC?)]
  [multC   (l  ExprC?) (r  ExprC?)]
  [lamC    (arg  symbol?) (body  ExprC?)] ; nomes não são mais necessários
  [appC    (fun  ExprC?) (arg  ExprC?)]
  [ifC     (condição  ExprC?) (sim  ExprC?) (não  ExprC?)]
  [quoteC  (s symbol?)]
  [loadC   (s ExprC?)]
  [letrecC (s symbol?) (body ExprC?) (cal ExprC?)]
  )

; Expressões açucaradas
(define-type ExprS
  [numS    (n  number?)]
  [idS     (s  symbol?)]
  [lamS    (arg  symbol?) (body  ExprS?)] ; muda de acordo
  [appS    (fun  ExprS?) (arg  ExprS?)]
  [plusS   (l  ExprS?) (r  ExprS?)]
  [bminusS (l  ExprS?) (r  ExprS?)]
  [uminusS (e  ExprS?)]
  [multS   (l  ExprS?) (r  ExprS?)]
  [ifS     (c  ExprS?) (s  ExprS?) (n  ExprS?)]
  [quoteS  (s symbol?)]
  [loadS   (s ExprS?)]
  [letS    (s symbol?) (body ExprS?) (val ExprS?)]
  [letrecS (s symbol?) (body ExprS?) (cal ExprS?)]
  )

; Expressões valores
(define-type Value
  [numV  (n  number?)]
  [symV  (s symbol?)]
  [closV (arg  symbol?) (body  ExprC?) (env  list?)])

; Associção
(define-type Binding
  [bind (name  symbol?) (val  Value?)])

; Retirando o açúcar
(define (desugar as); ExprS => ExprC
  (type-case ExprS as
    [numS    (n)          (numC n)]
    [idS     (s)          (idC s)]
    [lamS    (a b)        (lamC a (desugar b))]
    [appS    (fun arg)    (appC (desugar fun) (desugar arg))]
    [plusS   (l r)        (plusC (desugar l) (desugar r))]
    [multS   (l r)        (multC (desugar l) (desugar r))]
    [bminusS (l r)        (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)          (multC (numC -1) (desugar e))]
    [ifS     (c s n)      (ifC (desugar c) (desugar s) (desugar n))]
    [quoteS  (s)          (quoteC s)]
    [loadS   (s)          (loadC (desugar s))]
    [letS    (s body val) (appC (lamC s (desugar body)) (desugar val))]
    [letrecS (s body cal) (letrecC s (desugar body) (desugar cal))]
    ))

#|
 | Funções para lidar com o enviroment
 |#

; A lista de associações é o Environment
(define mt-env empty)    ; Environment vazio    --> (mt-env)
(define extend-env cons) ; Extende o enviroment --> (extend-env (bind 'x (Value Y)))
(define dummy (closV 'x (idC 'x) '()))       ; Cria uma dummy para associar no inicio do letrec


#|
 | Procura se o símbolo "s" está inserido no enviroment "env" (lista de bindings)
 | Se achar, retorna o valor associado a "s"
 | Se não, retorna um erro
 |#
(define (lookup s env); [s : symbol] [env : Env]) => Value
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string s) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? s (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup s (rest env))])]))        ; vê no resto

#|
 | Operadores de Value
 |#

(define (num+ l r); Value x Value => Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* l r); Value x Value => Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

(define (read_file s)
  (local [(define line (read s))]
    (cond
      [(eof-object? line)
       (close-input-port s)]
      [else (begin (println (interpS line)) (read_file s))])))
  

; Interpretador
(define (interp a env); ExprC x Env => Value
  (type-case ExprC a
    ; Tipos básicos
    [numC   (n) (numV n)]
    [idC    (n) (lookup n env)]
    [quoteC (s) (symV s)]

    [loadC (s) (read_file (open-input-file (symbol->string (symV-s (interp s env)))))]
    
    [lamC (a b) (closV a b env)] ; definição de função captura o environment

    [appC (f a)
          (local ([define f-value (interp f env)]) ; f-value é um closV
            (interp (closV-body f-value)
                    (extend-env
                        (bind (closV-arg f-value) (interp a env))
                        (closV-env f-value)
                    )))]

    [letrecC (s body cal)
             (local ([define f-dummy (interp body (extend-env (bind s dummy) env))])
                 (begin (set-closV-env! f-dummy (extend-env (bind s f-dummy) env))
                 (interp cal (closV-env f-dummy))))]



    ; Operações básicas (+ * e if)
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]
    ))

; Parser
(define (parse s); [s : s-expression] => ExprS
  (cond
    [(number? s) (numS s)]
    [(symbol? s) (idS  s)] ; Pode ser um símbolo livre nas definições de função
    [(list? s)
     (let ([sl s])
       (case  (first sl)
         [(+)      (plusS (parse (second sl)) (parse (third sl)))]
         [(*)      (multS (parse (second sl)) (parse (third sl)))]
         [(-)      (bminusS (parse (second sl)) (parse (third sl)))]
         [(~)      (uminusS (parse (second sl)))]
         [(func)   (lamS (second sl) (parse (third sl)))]
         [(lambda) (lamS (second sl) (parse (third sl)))]
         [(call)   (appS (parse (second sl)) (parse (third sl)))]
         [(if)     (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]

         [(quote)  (quoteS (second sl))]

         [(let)    (letS (first(second sl)) (parse (third sl)) (parse (second(second sl))))]
         [(let*)   (letS (first(second sl))
                         (letS (first(third sl)) (parse (fourth sl)) (parse (second(third sl))))
                         (parse (second(second sl))))]

         [(letrec) (letrecS (first(first(second sl)))
                   (parse (second(first(second sl))))
                   (parse (third sl)))]

         [(load)   (loadS (parse (second sl)))]

         [else (error 'parse "Comando não encontrado")]))]
    [else (error 'parse "invalid input")]))


; Facilitador
(define (interpS s) (interp (desugar (parse s)) mt-env))

