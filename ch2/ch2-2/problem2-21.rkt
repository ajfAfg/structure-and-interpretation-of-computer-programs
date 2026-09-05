#lang sicp
(#%require (only racket/base displayln))

(define (square-list items)
    (if (null? items)
        nil
        (cons
            (expt (car items) 2)
            (square-list (cdr items)))))

(define (square-list_ items)
    (map (lambda (x) (expt x 2)) items))

; テスト
(displayln (square-list (list 1 2 3 4))) ; (1 4 9 16)
(displayln (square-list_ (list 1 2 3 4))) ; (1 4 9 16)
