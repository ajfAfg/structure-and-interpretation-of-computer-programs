#lang sicp
(#%require (only racket/base displayln))

(define (average x y)
    (/ (+ x y) 2)
)

(define (abs x)
    (cond ((< x 0) (- x))
          (else x)
    )
)

(define (square x)
    (* x x)
)

(define (sqrt x)
    (define (improve guess)
        (average guess (/ x guess))
    )

    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001)
    )

    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))
        )
    )
    (sqrt-iter 1.0)
)

; テスト
(displayln (sqrt 9)) ; 3.00009155413138
(displayln (sqrt (+ 100 37))) ; 11.704699917758145
(displayln (sqrt (+ (sqrt 2) (sqrt 3)))) ; 1.7739279023207892
(displayln (square (sqrt 1000))) ; 1000.000369924366
