#lang sicp
(#%require (only racket/base displayln))

(define (exp b n)
    (if (= n 1)
        b
        (* b (exp b (- n 1)))
    )
)

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )
    (define (try guess count)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                (begin
                    (displayln count) ; 収束するまでの再帰回数を標準出力
                    next
                )
                (try next (+ count 1))
            )
        )
    )
    (try first-guess 0)
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (average-damp f)
    (lambda (x)
        (average x (f x))
    )
)

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)
(define (id x) x)
(define (repeated f n)
    (if (<= n 0)
        id
        (compose f (repeated f (- n 1)))
    )
)

(define (nth-root x n)
    (fixed-point
        ((repeated average-damp (floor (log x 2))) ; `log 4 2 = 1.9999...` みたいな値が得られると死ぬけど面倒臭いので直さない
            (lambda (y) (/ x (exp y (- n 1))))
        )
        1.0
    )
)

; テスト
(define (range from to)
    (if (>= from to)
        nil
        (cons from (range (+ from 1) to))
    )
)

(for-each
    (lambda (x)
        (displayln (nth-root (exp 2.0 x) x))
        (newline)
    )
    (range 2 10)
)

; 実験
;(define (sqrt2 x)
;    (fixed-point
;        (average-damp (lambda (y) (/ x y)))
;        1.0
;    )
;)
;(define (sqrt3 x)
;    (fixed-point
;        (average-damp (lambda (y) (/ x (exp y 2))))
;        1.0
;    )
;)
;(define (sqrt4 x)
;    (fixed-point
;        ((repeated average-damp 2) (lambda (y) (/ x (exp y 3))))
;        1.0
;    )
;)
;(define (sqrt5 x)
;    (fixed-point
;        ((repeated average-damp 2) (lambda (y) (/ x (exp y 4))))
;        1.0
;    )
;)
;(define (sqrt6 x)
;    (fixed-point
;        ((repeated average-damp 2) (lambda (y) (/ x (exp y 5))))
;        1.0
;    )
;)
;(define (sqrt7 x)
;    (fixed-point
;        ((repeated average-damp 2) (lambda (y) (/ x (exp y 6))))
;        1.0
;    )
;)
;(define (sqrt8 x)
;    (fixed-point
;        ((repeated average-damp 3) (lambda (y) (/ x (exp y 7))))
;        1.0
;    )
;)
;(define (sqrt9 x)
;    (fixed-point
;        ((repeated average-damp 3) (lambda (y) (/ x (exp y 8))))
;        1.0
;    )
;)
;(define (sqrt10 x)
;    (fixed-point
;        ((repeated average-damp 3) (lambda (y) (/ x (exp y 9))))
;        1.0
;    )
;)
;
;(displayln (sqrt2 4.0))
;(newline)
;(displayln (sqrt3 8.0))
;(newline)
;(displayln (sqrt4 16.0))
;(newline)
;(displayln (sqrt5 32.0))
;(newline)
;(displayln (sqrt6 64.0))
;(newline)
;(displayln (sqrt7 128.0))
;(newline)
;(displayln (sqrt8 256.0))
;(newline)
;(displayln (sqrt9 512.0))
;(newline)
;(displayln (sqrt10 1024.0))
;(newline)
