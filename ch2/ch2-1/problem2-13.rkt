#lang sicp
(#%require (only racket/base displayln))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
    (make-center-width c (* c p)))

(define (percent i)
    (/ (width i) (center i)))

(define (percent_ i1 i2)
    (+ (percent i1) (percent i2)))

; テスト
(define x (make-center-percent 6.8 0.01))
(define y (make-center-percent 6.8 0.005))

(displayln (percent (mul-interval x y)))
(displayln (percent_ x y))
