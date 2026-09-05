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

(define (reverse sequence)
    (fold-right
        (lambda (x y) (append y (list x)))
        nil
        sequence))

(define (reverse_ sequence)
    (fold-left
        (lambda (x y) (cons y x))
        nil
        sequence))

; テスト
(reverse  (list 1 2 3)) ; (3 2 1)
(reverse_ (list 1 2 3)) ; (3 2 1)
