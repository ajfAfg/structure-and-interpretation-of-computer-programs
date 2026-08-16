#lang sicp
(#%require (only racket/base void))

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

; Racket の random は 4294967087 以下の整数しか受け取れないので、
; それより大きな範囲の乱数は 4294967087 進法の桁ごとに生成して組み立てる
(define random-max 4294967087)
(define (big-random n)
    (if (<= n random-max)
        (random n)
        (+ (random random-max)
           (* random-max (big-random (quotient n random-max)))
        )
    )
)

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
    )
    (try-it (+ 1 (big-random (- n 1))))
)

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)
    )
)

; テスト
(define times 10)

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
)
(define (start-prime-test n start-time)
    (if (fast-prime? n times)
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
              ((fast-prime? from times) (timed-prime-test from) (loop (+ from 2) to (+ count 1)))
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
