#lang sicp
(#%require (only racket/base void displayln))

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder
                (square (expmod base (/ exp 2) m))
                m
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

(define (fermat-test-all n)
    (define (try-it a)
        (= (expmod a n n) a)
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
(define (report-fermat-test-all n)
    (display n)
    (display ": ")
    (displayln (fermat-test-all n))
)

; 期待せず素数と判定される
(for-each report-fermat-test-all (list 561 1105 1729 2465 2821 6601))

; 期待通り素数と判定される
(for-each report-fermat-test-all (list 2 3 5 7 11))

; 期待通り素数と判定されない
(for-each report-fermat-test-all (list 4 27))
