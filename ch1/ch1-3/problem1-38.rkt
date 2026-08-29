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

; テスト
(displayln
    (+
        (cont-frac
            (lambda (i) 1.0)
            (lambda (i)
                (if (= (remainder i 3) 2)
                    (* (ceiling (/ i 3)) 2)
                    1
                )
            )
            10
        )
        2
    )
) ; 2.7182817182817183
