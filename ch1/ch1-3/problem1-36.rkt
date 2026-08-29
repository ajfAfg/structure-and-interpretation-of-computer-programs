#lang sicp
(#%require (only racket/base displayln))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )
    (define (try guess)
        (displayln guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next)
            )
        )
    )
    (try first-guess)
)

; テスト
(define (average x y)
    (/ (+ x y) 2)
)

(displayln
    (fixed-point
        (lambda (x) (/ (log 1000) (log x)))
        2.0
    )
) ; 4.555532270803653、ステップ数は 35

(newline)

(displayln
    (fixed-point
        (lambda (x) (average x (/ (log 1000) (log x))))
        2.0
    )
) ; 4.555537551999825、ステップ数は 10
