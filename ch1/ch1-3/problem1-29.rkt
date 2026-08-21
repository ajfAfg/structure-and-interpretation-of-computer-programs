#lang sicp
(#%require (only racket/base displayln))

(define (cube x) (* x x x))

(define (sum term a next b)
    (if (> a b)
        0
        (+
            (term a)
            (sum term (next a) next b)
        )
    )
)

(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx)
    )
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

; シンプソンの公式による
(define (integral_ f a b n)
    (define h (/ (- b a) n))
    (define (y k)
        (f (+ a (* k h)))
    )
    (define (g i)
        (*
            (cond ((= i 0) 1)
                  ((= i n) 1)
                  ((= (remainder i 2) 0) 2)
                  (else 4)
            )
            (y i)
        )
    )
    (*
        (/ h 3.0)
        (sum g 0 (lambda (x) (+ x 1)) n)
    )
)

; テスト
(displayln (integral cube 0 1 0.01))
(displayln (integral cube 0 1 0.001))

(newline)

(displayln (integral_ cube 0 1 100))
(displayln (integral_ cube 0 1 1000))
