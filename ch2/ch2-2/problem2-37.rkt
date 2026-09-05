#lang sicp
(#%require (only racket/base displayln))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons
            (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
    (accumulate-n
        (lambda (v acc) (cons v acc))
        nil
        mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map
            (lambda (v)
                (map (lambda (w) (dot-product v w)) cols))
            m)))

; テスト
(define matrix1
    (list
        (list 1 2 3 4)
        (list 4 5 6 6)
        (list 6 7 8 9)))

(define matrix2
    (list
        (list  1  2  3)
        (list  4  5  6)
        (list  7  8  9)
        (list 10 11 12)))

; (30 56 80)
(displayln (matrix-*-vector matrix1 (list 1 2 3 4)))

; ((1 4 6)
;  (2 5 7)
;  (3 6 8)
;  (4 6 9))
(displayln (transpose matrix1))

; (( 70  80 90)
;  (126 147 168)
;  (180 210 240))
(displayln (matrix-*-matrix matrix1 matrix2))
