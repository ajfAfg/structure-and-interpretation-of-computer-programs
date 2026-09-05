#lang sicp
(#%require (only racket/base displayln))

(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

; テスト
(displayln (car (cons 1 2))) ; 1

(displayln (cdr (cons 1 2))) ; 2

(displayln (cdr (car (cons (cons 1 2) 3)))) ; 2
