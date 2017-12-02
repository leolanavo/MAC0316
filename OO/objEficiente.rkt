#lang plai
(define obj
  ;agora criamos os métodos (fechamentos) apenas uma vez
  ;objeto apenas seleciona um dos métodos já criados conforme a mensagem
  (letrec ([valor 0]
        [_add (lambda(parlist)(begin (set! valor (+ valor (car parlist)))
                                        valor))]
        [_sub(lambda(parlist)(begin (set! valor (- valor (car parlist)))
                                      valor))]
        [_valor(lambda(parlist) valor)]);first we need to create the environment
    (lambda (msg  parlist)
      ((case msg
         [(add) _add]
         [(sub) _sub]
         [(valor)_valor]
         )
       parlist)))
  )
(obj 'add '(5))
(obj 'sub '(4))
(obj 'valor '())
   