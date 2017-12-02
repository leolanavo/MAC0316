#lang racket

(define curry
  (lambda (f)
    (lambda (x)
      (lambda (y) (f x y)))))

(define mapcar
  (lambda (f l)
  (if (null? l) '()
    (cons (f (car l)) (mapcar f (cdr l))))))

(define mapc (curry mapcar))

(define combine
  (lambda (f1 f2 zero)
    (lambda (l)
      (if (null? l) zero
          (f2 (f1 (car l)) ((combine f1 f2 zero) (cdr l)))))))

(define id
  (lambda (x) x))

(define (cdr* l)
  (mapcar cdr l))

(define (mkpairfn x l)
  (mapcar (lambda (l) (cons x l)) l))

(define (append l1 l2)
  ((combine id cons l2) l1))


(cdr* '((1 2 3) (3 4) (5)))
(mkpairfn 'a '((1 2 3) (3 4) (5)))
(append '(1 2 3) '(4 5 6))