#lang sicp
(#%require (only racket/base displayln))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
        (error "div-interval: cannot divide in an interval that crosses zero: " y)
        (mul-interval
            x
            (make-interval (/ 1.0 (upper-bound y))
                        (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
    (make-center-width c (* c p)))

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval
            one
            (add-interval (div-interval one r1)
                          (div-interval one r2)))))

; テスト
(define r1 (make-center-percent 100 0.01))
(define r2 (make-center-percent 100 0.5))

(displayln (par1 r1 r1)) ; (48.51980198019802 . 51.52020202020203)
(displayln (par2 r1 r1)) ; (49.49999999999999 . 50.5)

(displayln (par1 r2 r2)) ; (8.333333333333334 . 225.0)
(displayln (par2 r2 r2)) ; (25.0 . 75.0)
