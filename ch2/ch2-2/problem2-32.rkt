#lang sicp
(#%require (only racket/base displayln))

(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append
                rest
                (map (lambda (s_) (cons (car s) s_)) rest)))))

; テスト
(displayln (subsets (list 1 2 3))) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
