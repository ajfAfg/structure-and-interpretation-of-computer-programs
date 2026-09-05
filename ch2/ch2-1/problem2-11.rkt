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

(define (mul-interval_ x y)
    (let ((a1 (lower-bound x))
          (b1 (upper-bound x))
          (a2 (lower-bound y))
          (b2 (upper-bound y)))
        (let ((c11 (and (< a1 0) (< b1 0)))
              (c12 (and (< a1 0) (>= b1 0)))
              (c13 (and (>= a1 0) (>= b1 0)))
              (c21 (and (< a2 0) (< b2 0)))
              (c22 (and (< a2 0) (>= b2 0)))
              (c23 (and (>= a2 0) (>= b2 0))))
            (cond ((and c11 c21) (make-interval (* b1 b2) (* a1 a2)))
                  ((and c11 c22) (make-interval (* a1 b2) (* a1 a2)))
                  ((and c11 c23) (make-interval (* a1 b2) (* b1 a2)))
                  ((and c12 c21) (make-interval (* b1 a2) (* a1 a2)))
                  ((and c12 c22) (make-interval (min (* a1 b2) (* b1 a2)) (max (* a1 a2) (* b1 b2))))
                  ((and c12 c23) (make-interval (* a1 b2) (* b1 b2)))
                  ((and c13 c21) (make-interval (* b1 a2) (* a1 b2)))
                  ((and c13 c22) (make-interval (* b1 a2) (* b1 b2)))
                  ((and c13 c23) (make-interval (* a1 a2) (* b1 b2)))))))

; テスト
(define (challenge p1 p2)
    (let ((x (make-interval (car p1) (cdr p1)))
          (y (make-interval (car p2) (cdr p2))))
        (let ((expected (mul-interval x y))
              (actual (mul-interval_ x y)))
            (if (not (equal? expected actual))
                (error "error: p1 / p2 / actual / expected =" p1 p2 actual expected)))))

(for-each
    (lambda (p)
        (challenge (car p) (cdr p)))
    (list
        (cons (cons -10 -5) (cons -20 -15))
        (cons (cons -10 -5) (cons -20  15))
        (cons (cons -10 -5) (cons  15  20))

        (cons (cons -10 5) (cons -20 -15))
        (cons (cons -10 5) (cons -20  15))
        (cons (cons -10 5) (cons  15  20))

        (cons (cons 5 10) (cons -20 -15))
        (cons (cons 5 10) (cons -20  15))
        (cons (cons 5 10) (cons  15  20))))
