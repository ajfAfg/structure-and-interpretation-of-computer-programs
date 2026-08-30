#lang sicp
(#%require (only racket/base displayln))

(define (power x n)
    (if (= n 1)
        x
        (* x (power x (- n 1)))
    )
)

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

(define (segment-length s)
    (let ((p1 (start-segment s))
          (p2 (end-segment s))
         )
         (sqrt
            (+
                (power (- (x-point p2) (x-point p1)) 2)
                (power (- (y-point p2) (y-point p1)) 2)
            )
         )
    )
)

; 長方形1
(define (make-rectangle side1 side2) (cons side1 side2))
(define (side1 r) (car r))
(define (side2 r) (cdr r))
(define (rectangle-length r) (segment-length (side1 r)))
(define (rectangle-width r) (segment-length (side2 r)))

(define (perimeter r)
    (*
        (+ (rectangle-length r) (rectangle-width r))
        2
    )
)

(define (area r)
    (* (rectangle-length r) (rectangle-width r))
)

; 長方形2
(define (make-rectangle_ point length width angle) (list point length width angle))
(define (rectangle-point_  r) (list-ref r 0))
(define (rectangle-length_ r) (list-ref r 1))
(define (rectangle-width_  r) (list-ref r 2))
(define (rectangle-angle_  r) (list-ref r 3))

(define (perimeter_ r)
    (*
        (+ (rectangle-length_ r) (rectangle-width_ r))
        2
    )
)

(define (area_ r)
    (* (rectangle-length_ r) (rectangle-width_ r))
)

; テスト1
(define r1
    (make-rectangle
        (make-segment (make-point 1 0) (make-point 3 0))
        (make-segment (make-point 0 2) (make-point 0 4))
    )
)
(displayln (perimeter r1)) ; 8
(displayln (area r1)) ; 4

(define r2
    (make-rectangle
        (make-segment (make-point 0 0) (make-point 3 4))
        (make-segment (make-point 0 0) (make-point -4 3))
    )
)
(displayln (perimeter r2)) ; 20
(displayln (area r2)) ; 25

; テスト2
(define r1_
    (make-rectangle_ (make-point 1 0) 2 2 0)
)
(displayln (perimeter_ r1_)) ; 8
(displayln (area_ r1_)) ; 4

(define r2_
    (make-rectangle_ (make-point 0 0) 5 5 0.92729521799931236) ; 0.92729521799931236 ≒ arctan (4/3)
)
(displayln (perimeter_ r2_)) ; 20
(displayln (area_ r2_)) ; 25
