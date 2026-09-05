#lang sicp
(#%require (only racket/base displayln))
(#%require math/number-theory)

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car z)
    (max-dividing-power 2 z))

(define (cdr z)
    (max-dividing-power 3 z))

; テスト
(displayln (car (cons 1 2))) ; 1

(displayln (cdr (cons 1 2))) ; 2

(displayln (cdr (car (cons (cons 1 2) 3)))) ; 2
