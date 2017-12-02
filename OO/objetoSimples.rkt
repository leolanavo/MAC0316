#lang plai
(define obj
  (let ([valor 0]);first we need to create the environment
    (lambda (msg  parlist)
      ((case msg
         [(add)(lambda(parlist)(begin (set! valor (+ valor (car parlist)))
                                valor))]
         [(sub)(lambda(parlist)(begin (set! valor (- valor (car parlist)))
                                 valor))]
         [(valor) (lambda(parlist) valor)]
         )
       parlist)))
  )
