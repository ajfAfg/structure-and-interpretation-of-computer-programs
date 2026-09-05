#lang sicp
(#%require (only racket/base displayln))

(define (last-pair items)
    (cond ((null? items) (error "empty list"))
          ((null? (cdr items)) (list (car items)))
          (else (last-pair (cdr items)))))

; テスト
(displayln (last-pair (list 23 72 149 34))) ; (34)
