#lang sicp
(#%require (only racket/base displayln))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
    (accumulate
        +
        0
        (map
            (lambda (x) (if (number? x) 1 (count-leaves x)))
            t)))

; テスト
(define x (cons (list 1 2) (list 3 4)))
(displayln (count-leaves x)) ; 4
(displayln (count-leaves (list x x))) ; 8
