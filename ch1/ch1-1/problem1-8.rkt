#lang sicp
(#%require (only racket/base displayln))

(define (abs x)
    (cond ((< x 0) (- x))
          (else x)
    )
)

(define (power x n)
    (cond ((= n 1) x)
          (else (* (power x (- n 1)) x))
    )
)

(define (improve guess x)
    (/
        (+
            (/ x (power guess 2))
            (* 2 guess)
        )
        3
    )
)

(define (good-enough? guess x)
    (< (abs (- (power guess 3) x)) 0.001)
)

(define (cubic-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cubic-root-iter (improve guess x) x)
    )
)

(define (cubic-root x)
    (cubic-root-iter 1.0 x)
)

; テスト
(displayln (cubic-root 2)) ; 1.259933493449977
(displayln (cubic-root 3)) ; 1.4422497895989996
(displayln (cubic-root 4)) ; 1.5874096961416333
(displayln (cubic-root 5)) ; 1.7100597366002945
