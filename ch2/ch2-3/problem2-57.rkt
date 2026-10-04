#lang sicp
(#%require (only racket/base displayln))

; 代数式
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 . as) (cons '+ (cons a1 as)))
(define (make-product m1 . ms) (cons '* (cons m1 ms)))
(define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (if (= (length (cddr s)) 1) (caddr s) (cons '+ (cddr s))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (if (= (length (cddr p)) 1) (caddr p) (cons '* (cddr p))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; 微分
(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
          ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))))
          ((exponentiation? exp)
            (make-product (exponent exp)
                          (make-product
                            (make-exponentiation
                                (base exp)
                                (make-sum (exponent exp) -1))
                            (deriv (base exp) var))))
          (else
            (error "unknown expression type: DERIV" exp))))

; テスト
(deriv (make-sum 1 'x 2) 'x) ; (+ 0 (+ 1 0))
(deriv (make-product 1 'x 2) 'x) ; (+ (* 1 (+ (* x 0) (* 1 2))) (* 0 (* x 2)))

(deriv '(* x y (+ x 3)) 'x) ; (+ (* x (+ (* y (+ 1 0)) (* 0 (+ x 3)))) (* 1 (* y (+ x 3))))
