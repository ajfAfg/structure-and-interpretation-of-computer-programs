#lang sicp
(#%require (only racket/base displayln))

(define (square-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))

(define (square-tree_ tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree tree))
          (else (map square-tree_ tree))))

; テスト
(define tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(displayln (square-tree  tree)) ; (1 (4 (9 16) 25) (36 49))
(displayln (square-tree_ tree)) ; (1 (4 (9 16) 25) (36 49))
