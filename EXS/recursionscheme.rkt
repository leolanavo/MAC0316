#lang lazy

(define impares
  (cons 1 (map (lambda (x) (+ x 2)) impares)))

(define fibs
  (cons 1 (cons 1 (map (lambda (a b) (+ a b)) (cdr fibs) fibs))))

(cadr impares)
(cadddr (cddddr fibs))