#lang racket

(define (combine-continuation-aux l f1 f2 pred zero cont)
    (if (null? l) (cont zero)
        (if (pred (car l)) zero
            (combine-continuation-aux (cdr l) f1 f2 pred zero (lambda (n) (cont (f2 (f1 (car l)) n)))))))

(define (combine-continuation-aux-call l f1 f2 pred zero cont)
  (call/cc (lambda (exit)
             (letrec ((ccauc*
                       (lambda (lista)
                         (if (null? lista) zero
                             (if (pred (car lista)) (exit zero)
                                 (f2 (f1 (car lista)) (ccauc* (cdr lista)))))))) (ccauc* l)))))

(define (combine-continuation f1 f2 pred zero)
  (lambda (l) (combine-continuation-aux l f1 f2 pred zero (lambda (x) x))))

(define (combine-continuation-call f1 f2 pred zero)
  (lambda (l) (combine-continuation-aux-call l f1 f2 pred zero (lambda (x) x))))

(define soma1
  (lambda (x) (+ x 1)))

((combine-continuation soma1 cons null? '()) '(1 2 3))
((combine-continuation-call soma1 cons zero? '()) '(1 2 3))