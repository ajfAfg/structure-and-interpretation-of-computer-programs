#lang sicp
(#%require (only racket/base displayln))

(define (reverse items)
    (define (iter items acc)
        (if (null? items)
            acc
            (iter (cdr items) (cons (car items) acc))))
    (iter items nil))

(define (list? x)
    (or (null? x) (pair? x)))

(define (deep-reverse x)
    (if (list? x)
        (reverse (map deep-reverse x))
        x))

; テスト
(define x
    (list (list 1 2) (list 3 4)))

(displayln (reverse x)) ; ((3 4) (1 2))
(displayln (deep-reverse x)) ; ((4 3) (2 1))
