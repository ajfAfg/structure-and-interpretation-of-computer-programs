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
    (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

(define (width-interval x)
    (/ (- (upper-bound x) (lower-bound x)) 2))

; テスト

; 任意の区間 [a1,b1], [a2,b2] について、
;
; width([a1,b1]) + width([a2,b2])
; = (b1-a1)/2 + (b2-a2)/2
; = ((b1+b2)-(a1+a2))/2
; = width([a1,b1]+[a2,b2])
;
; width([a1,b1]) + width([a2,b2])
; = (b1-a1)/2 + (b2-a2)/2
; = ((b1-a2)-(a1-b2))/2
; = width([a1,b1]-[a2,b2])

(define x1 (make-interval 0 1))
(define y1 (make-interval 0 1))
(define x2 (make-interval 1 2))
(define y2 (make-interval 1 2))

(displayln (width-interval (mul-interval x1 y1))) ; 1/2
(displayln (* (width-interval x1) (width-interval y1))) ; 1/4
(displayln (width-interval (mul-interval x2 y2))) ; 3/2
(displayln (* (width-interval x2) (width-interval y2))) ; 1/4
