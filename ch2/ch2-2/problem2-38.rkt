#lang sicp
(#%require (only racket/base displayln))

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

; テスト
(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6

(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)

; なお、op が交換律と結合律を満たす
; （もしくは、結合律を満たし、initial が単位元）ならば、
; fold-right と fold-left は任意の列に対して同じ値を返す。
