#lang sicp
(#%require (only racket/base displayln))

(define (inc x)
    (+ x 1)
)

(define (product term a next b)
    (if (> a b)
        1
        (*
            (term a)
            (product term (next a) next b)
        )
    )
)

(define (product_ term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))
        )
    )
    (iter a 1)
)

(define (factorial x)
    (product (lambda (x) x) 1 inc x)
)

(define (factorial_ x)
    (product_ (lambda (x) x) 1 inc x)
)

(define pi
    (*
        4.0
        (/
            (product (lambda (x) (* (+ (floor (/ x 2)) 1) 2)) 1 inc 10000)
            (product (lambda (x) (+ (* (ceiling (/ x 2)) 2) 1)) 1 inc 10000)
        )
    )
)

(define pi_
    (*
        4.0
        (/
            (product_ (lambda (x) (* (+ (floor (/ x 2)) 1) 2)) 1 inc 10000)
            (product_ (lambda (x) (+ (* (ceiling (/ x 2)) 2) 1)) 1 inc 10000)
        )
    )
)

; テスト
(displayln (factorial 10)) ; 3,628,800
(displayln pi) ; 3.1417497057380523

(displayln (factorial_ 10)) ; 3,628,800
(displayln pi_) ; 3.1417497057380523
