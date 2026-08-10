#lang sicp

; 再帰プロセスによる
(define (f n)
    (cond ((< n 3) n)
          (else
            (+
                (f (- n 1))
                (* 2 (f (- n 2)))
                (* 3 (f (- n 3)))
            )
          )
    )
)

; 反復プロセスによる
(define (g n)
    (define (g-iter n1 n2 n3 count)
        (cond ((= count 0) n3)
              (else
                (g-iter
                    (+ n1 (* 2 n2) (* 3 n3))
                    n1
                    n2
                    (- count 1)
                )
              )
        )
    )
    (cond ((< n 0) n)
          (else (g-iter 2 1 0 n))
    )
)

; テスト
(define (challenge n)
    (let ((x (f n))
          (y (g n)))
        (if (not (= x y))
            (error "error: n / f(n) / g(n) =" n x y)
        )
    )
)

(define (run-challenge from to)
    (cond ((<= from to)
           (challenge from)
           (run-challenge (+ from 1) to))
    )
)

(run-challenge -5 10)
