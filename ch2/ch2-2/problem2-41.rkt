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

(define (unique-triples n)
    (flatmap
        (lambda (i)
            (flatmap
                (lambda (j)
                    (map
                        (lambda (k) (list i j k))
                        (enumerate-interval 1 (- j 1))))
                (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (sum seq)
    (accumulate + 0 seq))

(define (triples-with-sum triples s)
    (filter
        (lambda (t) (= (sum t) s))
        triples))

; テスト
(triples-with-sum (unique-triples 5) 8) ; ((4 3 1) (5 2 1))
