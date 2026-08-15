#lang sicp
(#%require (only racket/base displayln))

(define (even? n)
    (= (remainder n 2) 0)
)

; 線形時間かかる掛け算
(define (* a b)
    (if (= b 0)
        0
        (+ a (* a (- b 1)))
    )
)

; 対数時間かかる掛け算
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (*_ a b)
    (cond ((= b 0) 0)
          ((even? b) (double (*_ a (halve b))))
          (else (+ a (*_ a (- b 1))))
    )
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(define (challenge a b)
    (let ((expected (* a b))
          (actual (*_ a b)))
         (if (not (= expected actual))
             (error "error: a / b / actual / expected =" a b actual expected)
         )
    )
)

(define (run-challenge as bs)
    (for-each
        (lambda (a)
            (for-each (lambda (b) (challenge a b)) bs)
        )
        as
    )
)

(run-challenge (range -5 6) (range 0 11))
