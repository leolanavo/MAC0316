#lang lazy

(define impares
  (cons 1 (map (lambda (x) (+ x 2)) impares)))

(define fibs
  (cons 1 (cons (map (lambda (a b) (+ a b)) fibs) (cdr fibs))))

(cadr impares)
(fibs)