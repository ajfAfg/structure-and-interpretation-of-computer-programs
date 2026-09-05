#lang sicp
(#%require (only racket/base displayln))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
    (lambda (f) (lambda (x) (f x))))
(define two
    (lambda (f) (lambda (x) (f (f x)))))

(define (to-integer n)
    ((n (lambda (x) (+ x 1))) 0))

(define (add n m)
    (lambda (f) (lambda (x) ((n f) ((m f) x)))))

; テスト
(displayln (to-integer zero))
(displayln (to-integer (add-1 zero)))
(displayln (to-integer one))
(displayln (to-integer two))

(displayln (to-integer (add zero one)))
(displayln (to-integer (add two two)))
