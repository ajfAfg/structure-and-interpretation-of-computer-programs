#lang sicp
(#%require (only racket/base displayln))

(define (C n k)
    (cond ((or (= k 0) (= n k)) 1)
          (else
            (+
                (C (- n 1) (- k 1))
                (C (- n 1) k)
            )
          )
    )
)

; テスト
(define (challenge n k expected)
    (let ((x (C n k)))
        (if (not (= x expected))
            (error "error: C(n, k) / actual / expected =" n k x expected)
        )
    )
)

(challenge 0 0 1)

(challenge 1 0 1)
(challenge 1 1 1)

(challenge 2 0 1)
(challenge 2 1 2)
(challenge 2 2 1)

(challenge 3 0 1)
(challenge 3 1 3)
(challenge 3 2 3)
(challenge 3 3 1)

(challenge 4 0 1)
(challenge 4 1 4)
(challenge 4 2 6)
(challenge 4 3 4)
(challenge 4 4 1)

(challenge 5 0 1)
(challenge 5 1 5)
(challenge 5 2 10)
(challenge 5 3 10)
(challenge 5 4 5)
(challenge 5 5 1)
