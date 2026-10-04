#lang sicp
(#%require (only racket/base displayln))

(define (equal? x y)
    (cond ((and (null? x) (null? y)) true)
          ((and (pair? x) (pair? y))
            (and
                (equal? (car x) (car y))
                (equal? (cdr x) (cdr y))))
          ((eq? x y) true)
          (else false)))

; テスト
(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f
