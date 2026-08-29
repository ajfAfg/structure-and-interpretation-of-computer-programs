#lang sicp
(#%require (only racket/base displayln))

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)
(define (id x) x)
(define (repeated f n)
    (if (<= n 0)
        id
        (compose f (repeated f (- n 1)))
    )
)

(define (smooth f)
    (define dx 0.00001)
    (lambda (x)
        (/
            (+
                (f (- x dx))
                (f x)
                (f (+ x dx))
            )
            3
        )
    )
)

(define (n-fold-smooth n)
    (repeated smooth n)
)

; テスト
(define (square x) (* x x))

(displayln
    ((smooth square) 2)
) ; 4.000000000066667

(displayln
    (((n-fold-smooth 5) square) 2)
) ; 4.000000000333333

(displayln
    ((smooth abs) 0)
) ; 6.6666666666666675e-6

(displayln
    (((n-fold-smooth 3) abs) 0)
) ; 1.1111111111111112e-5
