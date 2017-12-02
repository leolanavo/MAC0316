#lang racket

(define conj (lambda (x) (= x 2)))

(define conj-nulo (lambda (x) #F))

(define membro (lambda (conj mem) (conj mem)))

(define (inclui-membro conj mem)
  (if (conj mem) conj
    (lambda (x) (or (conj x) (equal? x mem)))))

(define (uniao conj1 conj2) 
  (lambda (x) (or (conj1 x) (conj2 x))))