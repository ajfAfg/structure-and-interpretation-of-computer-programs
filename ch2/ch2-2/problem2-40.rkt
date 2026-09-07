#lang sicp
(#%require (only racket/base displayln))
(#%require (only math/number-theory prime?))

(define (filter pred? seq)
    (cond ((null? seq) nil)
          ((pred? (car seq))
            (cons (car seq) (filter pred? (cdr seq))))
          (else (filter pred? (cdr seq)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list
        (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (unique-pairs n)
    (flatmap
        (lambda (i)
            (map
                (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
    (map
        make-pair-sum
        (filter
            prime-sum?
            (unique-pairs n))))

; テスト
(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
