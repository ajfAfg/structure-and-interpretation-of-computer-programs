#lang sicp
(#%require (only racket/base displayln))

(define (cont-frac n d k)
    (define (f i)
        (if (< k i)
            0
            (/
                (n i)
                (+ (d i) (f (+ i 1)))
            )
        )
    )
    (f 1)
)

(define (tan-cf x k)
    (cont-frac
        (lambda (i)
            (if (= i 1)
                x
                (* x x -1)
            )
        )
        (lambda (i) (- (* 2 i) 1))
        k
    )
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(for-each
    (lambda (x)
        (displayln
            (tan-cf x 20)
        )
    )
    (range 1.0 20.0)
)
