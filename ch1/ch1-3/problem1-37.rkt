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

(define (cont-frac_ n d k)
    (define (iter i acc)
        (if (< i 1)
            acc
            (iter
                (- i 1)
                (/ (n i) (+ (d i) acc))
            )
        )
    )
    (iter k 0)
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(for-each
    (lambda (k)
        (displayln
            (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
        )
    )
    (range 1 20)
)

(newline)

(for-each
    (lambda (k)
        (displayln
            (cont-frac_ (lambda (i) 1.0) (lambda (i) 1.0) k)
        )
    )
    (range 1 20)
)
