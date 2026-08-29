#lang sicp
(#%require (only racket/base displayln))

(define (double f)
    (lambda (x) (f (f x)))
)

; テスト
(define (inc x) (+ x 1))

(displayln
    (((double (double double)) inc) 5)
) ; 21
