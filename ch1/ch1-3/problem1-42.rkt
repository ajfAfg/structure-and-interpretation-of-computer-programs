#lang sicp
(#%require (only racket/base displayln))

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)

; テスト
(define (inc x) (+ x 1))
(define (square x) (* x x))

(displayln
    ((compose square inc) 6)
) ; 49
