#lang plai
;a classe é uma fábrica de objetos do mesmo tio
(define Classe (lambda ()
  (letrec ([valor 0]
        [_add (lambda(parlist)(begin (set! valor (+ valor (car parlist)))
                                        valor))]
        [_sub(lambda(parlist)(begin (set! valor (- valor (car parlist)))
                                      valor))]
        [_valor(lambda(parlist) valor)];first we need to create the environment
        [objeto (lambda (msg  parlist)
                  ((case msg
                     [(add) _add]
                     [(sub) _sub]
                     [(valor) _valor]
                     )
                   parlist))])
       objeto))
  )
(define ob1 (Classe))
(define ob2 (Classe))
(ob1 'add '(1))
(ob1 'sub '(1))
(ob2 'add '(5))
(ob2 'sub '(1))
(ob1 'valor '())
(ob2 'valor '())