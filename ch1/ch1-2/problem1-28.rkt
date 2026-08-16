#lang sicp
(#%require (only racket/base void displayln))

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (let ((x (expmod base (/ exp 2) m)))
                (if (and (not (= x 1))
                         (not (= x (- m 1)))
                         (= (remainder (square x) m) 1))
                    0
                    (remainder (square x) m)
                )
            )
          )
          (else
            (remainder
                (* base (expmod base (- exp 1) m))
                m
            )
          )
    )
)

(define (miller-rabin-test-all n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1)
    )
    (define (loop a)
        (cond ((= a n) true)
              ((try-it a) (loop (+ a 1)))
              (else false)
        )
    )
    (loop 1)
)

; テスト
(define (report-miller-rabin-test-all n)
    (display n)
    (display ": ")
    (displayln (miller-rabin-test-all n))
)

; 期待通り素数と判定される
(for-each report-miller-rabin-test-all (list 2 3 5 7 11))

; 期待通り素数と判定されない
(for-each report-miller-rabin-test-all (list 4 27 561 1105 1729 2465 2821 6601))
