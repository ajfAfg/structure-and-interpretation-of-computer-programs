#lang sicp
(#%require (only racket/base void))

(define (square x) (* x x))

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

(define (prime? n)
    (= n (smallest-divisor n))
)

; テスト
(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
)
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))
    )
)
(define (timed-prime-test n)
    (display n)
    (start-prime-test n (runtime))
    (newline)
)

(define (do from to) ; assume (= (remainder from 2) 1)
    (define (loop from to count)
        (cond ((or (>= from to) (< 2 count)) (void))
              ((prime? from) (timed-prime-test from) (loop (+ from 2) to (+ count 1)))
              (else (loop (+ from 2) to count))
        )
    )
    (loop from to 0)
    (newline)
)

; テキストでは「1000、10,000、100,000 より大きな素数をそれぞれ 3 つ見つけよ」と書かれているが、
; 現代の機械ではどれも一瞬で見つかり実行時間の差が出ないので、
; 以下ではより大きな数を与えている。
(do (+ (expt 10 10) 1) (expt 10 11))
(do (+ (expt 10 11) 1) (expt 10 12))
(do (+ (expt 10 12) 1) (expt 10 13))
