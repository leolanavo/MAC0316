#lang racket
(define ClasseA (lambda ()
    (letrec ( [valorA 0]
              [func1 (lambda (parlist) (+ 5 (self 'm2 '())))]
              [func2 (lambda (parlist) 10)]

              [self (lambda (msg parlist)
                      (case msg
                        [(m1) (func1 parlist)]
                        [(m2) (func2 parlist)]))])
      self)))
(define ClasseB (lambda()
      (letrec ( [Superobj (ClasseA)]
                [valorB 0]
                [_m2 (lambda (parlist) 5)]
                [self (lambda (msg parlist)
                        (case msg
                          [(m2) (_m2 parlist)]
                          ;se método não está aqui, quem sabe no "super objeto"
                          ;ou seja, nos métodos associados à classe mãe.
                          [ else (Superobj msg parlist)]))])
        self)))
(define obA (ClasseA))
(define obB (ClasseB))
(obA 'm1 '());m1 da superclasse
(obA 'm2 '());m2 da s
(obA 'm2 '()); este executa o método da subclasse,otimo.
(obA 'm1 '()); aqui executamos o método da superclasse, mas m1 chama o m2 da própria superclasse
;gostaríamos que o comportamento fosse diferente, quando m1 chama m2, caso o receptor
;da mensagem seja da ClasseB, deveria  executar m2 da ClasseB.

