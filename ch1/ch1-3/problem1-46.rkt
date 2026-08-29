#lang sicp
(#%require (only racket/base displayln))

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough? improve-guess)
    (lambda (x)
        (define (try guess)
            (let ((next (improve-guess guess)))
                (if (good-enough? next guess)
                    next
                    (try next)
                )
            )
        )
        (try x)
    )
)

(define (sqrt x)
    (define (good-enough? guess _)
        (< (abs (- (square guess) x)) 0.001)
    )
    (define (improve guess)
        (average guess (/ x guess))
    )
    ((iterative-improve good-enough? improve) x)
)

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )
    ((iterative-improve close-enough? f) first-guess)
)

; テスト
(displayln (sqrt 9.0)) ; 3.000000001396984
(displayln (sqrt 1000000.0)) ; 1000.0000000000118

(displayln (fixed-point cos 1.0)) ; 0.7390822985224024
(displayln
    (fixed-point
        (lambda (y) (+ (sin y) (cos y)))
        1.0
    )
) ; 1.2587315962971173
