#lang sicp
(#%require (only racket/base displayln))

(define (sgn x)
    (cond ((> x 0) 1)
          ((= x 0) 0)
          (else -1)
    )
)

(define (make-rat n d)
    (let ((g (* (gcd n d) (sgn d))))
        (cons (/ n g) (/ d g))
    )
)
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline)
)

(define (add-rat x y)
    (make-rat
        (+
            (* (numer x) (denom y))
            (* (numer y) (denom x))
        )
        (* (denom x) (denom y))
    )
)

(define (sub-rat x y)
    (make-rat
        (-
            (* (numer x) (denom y))
            (* (numer y) (denom x))
        )
        (* (denom x) (denom y))
    )
)

(define (mul-rat x y)
    (make-rat
        (* (numer x) (numer y))
        (* (denom x) (denom y))
    )
)

(define (div-rat x y)
    (make-rat
        (* (numer x) (denom y))
        (* (denom x) (numer y))
    )
)

(define (equal-rat? x y)
    (=
        (* (numer x) (denom y))
        (* (numer y) (denom x))
    )
)

; テスト
(print-rat (make-rat 3 6))   ; 1/2
(print-rat (make-rat -3 6))  ; -1/2
(print-rat (make-rat 3 -6))  ; -1/2
(print-rat (make-rat -3 -6)) ; 1/2

(print-rat
    (sub-rat
        (make-rat 1 3)
        (make-rat 1 2)
    )
) ; -1/6

(print-rat
    (mul-rat
        (make-rat -1 2)
        (make-rat 2 3)
    )
) ; -1/3
