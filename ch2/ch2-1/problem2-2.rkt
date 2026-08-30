#lang sicp
(#%require (only racket/base displayln))

; 点
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
    (newline)
)

; 線分
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
    (let ((p1 (start-segment s))
          (p2 (end-segment s))
         )
         (make-point
            (/ (+ (x-point p1) (x-point p2)) 2)
            (/ (+ (y-point p1) (y-point p2)) 2)
         )
    )
)

; テスト
(print-point
    (midpoint-segment
        (make-segment (make-point 1 2) (make-point 3 4))
    )
) ; (2,3)

(print-point
    (midpoint-segment
        (make-segment (make-point -3 -4) (make-point 1 2))
    )
) ; (-1,-1)
