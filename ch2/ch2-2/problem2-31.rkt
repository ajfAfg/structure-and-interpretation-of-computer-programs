#lang sicp
(#%require (only racket/base displayln))

(define (tree-map proc tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (proc tree))
          (else (map (lambda (t) (tree-map proc t)) tree))))

(define (square x) (* x x))
(define (square-tree tree)
    (tree-map square tree))

; テスト
(define tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(displayln (square-tree tree)) ; (1 (4 (9 16) 25) (36 49))
