#lang sicp
(#%require (only racket/base displayln))

(define (filtered-accumulate pred combiner null-value term a next b)
    (if (and (> a b))
        null-value
        (combiner
            (if (pred a) (term a) null-value)
            (filtered-accumulate pred combiner null-value term (next a) next b)
        )
    )
)

; テスト
(define (add x y) (+ x y))
(define (multiply x y) (* x y))
(define (id x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (prime? n)
    (define (divides? a b)
        (= (remainder b a) 0)
    )
    (define (next x)
        (cond ((= x 2) 3)
            (else (+ x 2))
        )
    )
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))
        )
    )
    (define (smallest-divisor n)
        (find-divisor n 2)
    )

    (= n (smallest-divisor n))
)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)

(displayln
    (filtered-accumulate
        prime?
        add
        0
        square
        1
        inc
        20
    )
) ; 1028


(displayln
    (let ((n 20))
        (filtered-accumulate
            (lambda (i) (= (gcd i n) 1))
            multiply
            1
            id
            1
            inc
            (- n 1)
        )
    )
) ; 8729721
