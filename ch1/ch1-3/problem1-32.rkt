#lang sicp
(#%require (only racket/base displayln))

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate combiner null-value term (next a) next b)
        )
    )
)

(define (accumulate_ combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))
        )
    )
    (iter a null-value)
)

(define (add x y) (+ x y))
(define (multiply x y) (* x y))
(define (inc x) (+ x 1))
(define (id x) x)

(define (sum term a next b)
    (accumulate add 0 term a next b)
)
(define (sum_ term a next b)
    (accumulate_ add 0 term a next b)
)
(define (product term a next b)
    (accumulate multiply 1 term a next b)
)
(define (product_ term a next b)
    (accumulate_ multiply 1 term a next b)
)

; テスト
(define (challenge fun fun_ term a next b)
    (let ((expected (fun term a next b))
          (actual (fun_ term a next b)))
         (if (not (= expected actual))
             (error "error: fun / fun_ / term / a / next / b / actual / expected =" fun fun_ term a next b actual expected)
         )
    )
)

(challenge
    sum
    sum_
    (lambda (x) (* x x x))
    1
    inc
    10
)

(challenge
    sum
    sum_
    id
    1
    inc
    10
)

(challenge
    sum
    sum_
    (lambda (x) (/ 1 (* x (+ x 2)))) ; 浮動小数点数上の加算は結合法則を満たさない点に注意
    1
    (lambda (x) (+ x 4))
    1000
)

(challenge
    product
    product_
    id
    1
    inc
    10
)
