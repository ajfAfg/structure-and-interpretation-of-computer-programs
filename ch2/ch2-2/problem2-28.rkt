#lang sicp
(#%require (only racket/base displayln))

(define (list? x)
    (or (null? x) (pair? x)))

(define (flatten items)
    (if (null? items)
        nil
        (let ((x (car items))
              (xs (cdr items)))
            (if (list? x)
                (append x (flatten xs))
                (cons x (flatten xs))))))

(define (fringe tree)
    (cond ((null? tree) nil)
          ((pair? tree) (flatten (map fringe tree)))
          (else (list tree))))

; テスト
(displayln (flatten (list 1 2 3 4))) ; (1 2 3 4)
(displayln (flatten (list (list 1) 2 3 4))) ; (1 2 3 4)
(displayln (flatten (list (list (list 1)) 2 3 4))) ; ((1) 2 3 4)

(define x (list (list 1 2) (list 3 4)))

(displayln (fringe x)) ; (1 2 3 4)
(displayln (fringe (list x x))) ; (1 2 3 4 1 2 3 4)
