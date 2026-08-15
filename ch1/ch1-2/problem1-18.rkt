#lang sicp
(#%require (only racket/base displayln))

(define (even? n) (= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

; 再帰プロセスによる
(define (* a b)
    (cond ((= b 0) 0)
          ((even? b) (double (* a (halve b))))
          (else (+ a (* a (- b 1))))
    )
)

; 反復プロセスによる
(define (*_ a b)
    (define (*-iter a b acc)
        (cond ((= b 0) acc)
            ((even? b) (*-iter (double a) (halve b) acc))
            (else (*-iter a (- b 1) (+ a acc)))
        )
    )
    (*-iter a b 0)
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(define (challenge a b)
    (let ((expected (* a b))
          (actual (*_ a b)))
         (if (not (= expected actual))
             (error "error: a / b / actual / expected =" a b actual expected)
         )
    )
)

(define (run-challenge as bs)
    (for-each
        (lambda (a)
            (for-each (lambda (b) (challenge a b)) bs)
        )
        as
    )
)

(run-challenge (range -5 6) (range 0 11))
