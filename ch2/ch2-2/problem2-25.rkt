#lang sicp
(#%require (only racket/base displayln))

(define items1 (list 1 3 (list 5 7) 9))
(define items2 (list (list 7)))
(define items3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(displayln
    (car (cdr (car (cdr (cdr items1)))))) ; 7

(displayln
    (car (car items2))) ; 7

(displayln
    (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items3))))))))))))) ; 7
