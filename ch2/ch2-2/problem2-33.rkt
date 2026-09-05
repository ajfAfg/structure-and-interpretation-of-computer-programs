#lang sicp
(#%require (only racket/base displayln))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (_ acc) (+ acc 1)) 0 sequence))

; テスト
(displayln (map (lambda (x) (* x x)) (list 1 2 3))) ; (1 4 9)
(displayln (append (list 1 2 3) (list 4 5 6))) ; (1 2 3 4 5 6)
(displayln (length (list 1 2 3))) ; 3
