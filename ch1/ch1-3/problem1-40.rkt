#lang sicp
(#%require (only racket/base displayln))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next)
            )
        )
    )
    (try first-guess)
)

(define (deriv g)
    (define dx 0.00001)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x)) dx)
    )
)

(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))
    )
)

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess)
)

(define (cubic a b c)
    (lambda (x)
        (+
            (* x x x)
            (* a x x)
            (* b x)
            c
        )
    )
)

; テスト
(displayln (newtons-method (cubic 0 0 -8) 1.0)) ; 2.000000000036784
(displayln (newtons-method (cubic -6 11 -6) 1.0)) ; 1.0
(displayln (newtons-method (cubic -6 11 -6) 10.0)) ; 3.0000000000002762
