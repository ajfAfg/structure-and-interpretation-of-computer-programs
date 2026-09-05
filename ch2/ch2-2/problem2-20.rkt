#lang sicp
(#%require (only racket/base displayln))

(define (filter pred? items)
    (if (null? items)
        nil
        (if (pred? (car items))
            (cons (car items) (filter pred? (cdr items)))
            (filter pred? (cdr items)))))

(define (same-parity x . items)
    (cons
        x
        (filter
            (lambda (y) (= (remainder x 2) (remainder y 2)))
            items)))

; テスト
(displayln (same-parity 1 2 3 4 5 6 7)) ; (1 3 5 7)
(displayln (same-parity 2 3 4 5 6 7)) ; (2 4 6)
