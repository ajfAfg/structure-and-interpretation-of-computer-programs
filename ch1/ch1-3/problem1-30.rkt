#lang sicp
(#%require (only racket/base displayln))

(define (sum term a next b)
    (if (> a b)
        0
        (+
            (term a)
            (sum term (next a) next b)
        )
    )
)

(define (sum_ term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))
        )
    )
    (iter a 0)
)

; テスト
(define (challenge term a next b)
    (let ((expected (sum term a next b))
          (actual (sum_ term a next b)))
         (if (not (= expected actual))
             (error "error: term / a / next / b / actual / expected =" term a next b actual expected)
         )
    )
)

(challenge
    (lambda (x) (* x x x))
    1
    (lambda (x) (+ x 1))
    10
)

(challenge
    (lambda (x) x)
    1
    (lambda (x) (+ x 1))
    10
)

(challenge
    (lambda (x) (/ 1 (* x (+ x 2)))) ; 浮動小数点数上の加算は結合法則を満たさない点に注意
    1
    (lambda (x) (+ x 4))
    1000
)
