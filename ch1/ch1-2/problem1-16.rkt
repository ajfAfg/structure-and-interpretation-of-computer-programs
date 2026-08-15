#lang sicp
(#%require (only racket/base displayln))

(define (square x)
    (* x x)
)

(define (even? n)
    (= (remainder n 2) 0)
)

; 再帰プロセスによる
(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))
    )
)

; 反復プロセスによる
(define (fast-expt_ b n)
    (define (fast-expt-iter b n a)
        (cond ((= n 0) a)
              ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
              (else (fast-expt-iter b (- n 1) (* a b)))
        )
    )
    (fast-expt-iter b n 1)
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(define (challenge b n)
    (let ((expected (fast-expt b n))
          (actual (fast-expt_ b n)))
         (if (not (= expected actual))
             (error "error: b / n / actual / expected =" b n actual expected)
         )
    )
)

(define (run-challenge bs ns)
    (for-each
        (lambda (b)
            (for-each (lambda (n) (challenge b n)) ns)
        )
        bs
    )
)

(run-challenge (range -5 6) (range 0 11))
