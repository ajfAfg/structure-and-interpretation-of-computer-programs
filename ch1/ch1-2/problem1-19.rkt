#lang sicp
(#%require (only racket/base displayln))

(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))

; 線形時間かかる定義
(define (fib n)
    (define (fib-iter a b count)
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))
        )
    )
    (fib-iter 1 0 n)
)

; 対数時間かかる定義
(define (fib_ n)
    (define (fib-iter a b p q count)
        (cond ((= count 0) b)
              ((even? count)
                (fib-iter
                    a
                    b
                    (+ (square p) (square q))
                    (+ (square q) (* 2 p q))
                    (/ count 2)
                )
              )
              (else
                (fib-iter
                    (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- count 1)
                )
              )
        )
    )
    (fib-iter 1 0 0 1 n)
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(define (challenge n)
    (let ((expected (fib n))
          (actual (fib_ n)))
         (if (not (= expected actual))
             (error "error: n / actual / expected =" n actual expected)
         )
    )
)

(define (run-challenge ns)
    (for-each challenge ns)
)

(run-challenge (range 0 11))
