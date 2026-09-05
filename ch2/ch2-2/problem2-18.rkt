#lang sicp
(#%require (only racket/base displayln))

(define (reverse items)
    (define (iter items acc)
        (if (null? items)
            acc
            (iter (cdr items) (cons (car items) acc))))
    (iter items nil))

; テスト
(displayln (reverse (list 1 4 9 16 25))) ; (25 16 9 4 1)
