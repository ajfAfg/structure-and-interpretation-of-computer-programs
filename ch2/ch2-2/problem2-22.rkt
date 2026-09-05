#lang sicp
(#%require (only racket/base displayln))

(define (square-list items)
    (define (iter items acc)
        (if (null? items)
            acc
            (iter
                (cdr items)
                (cons (expt (car items) 2) acc))))
    (reverse (iter items nil)))

; テスト
(displayln (square-list (list 1 2 3 4))) ; (1 4 9 16)
