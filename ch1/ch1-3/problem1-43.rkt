#lang sicp
(#%require (only racket/base displayln))

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)
(define (id x) x)

(define (repeated f n)
    (lambda (x)
        (define (iter n)
            (if (<= n 0)
                x
                (f (iter (- n 1)))
            )
        )
        (iter n)
    )
)

(define (repeated_ f n)
    (if (<= n 0)
        id
        (compose f (repeated_ f (- n 1)))
    )
)

; テスト
(define (square x) (* x x))

(displayln
    ((repeated square 2) 5)
) ; 625

(displayln
    ((repeated_ square 2) 5)
) ; 625
