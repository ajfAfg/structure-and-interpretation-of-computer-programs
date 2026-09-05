#lang sicp
(#%require (only racket/base displayln))

; Mobile
(define (make-mobile left right)
    (cons left right))

(define (left-branch mobile)
    (car mobile))
(define (right-branch mobile)
    (cdr mobile))

; Branch
(define (make-branch length structure)
    (cons length structure))

(define (branch-length branch)
    (car branch))
(define (branch-structure branch)
    (cdr branch))

; Procedures
(define (total-weight mobile)
    (+
        (total-weight_ (left-branch mobile))
        (total-weight_ (right-branch mobile))))
(define (total-weight_ branch)
    (let ((x (branch-structure branch)))
        (if (number? x)
            x
            (total-weight x))))

(define (balanced? mobile)
    (let ((b1 (left-branch mobile))
          (b2 (right-branch mobile)))
        (let ((s1 (branch-structure b1))
              (s2 (branch-structure b2)))
            (and
                (=
                    (* (total-weight_ b1) (branch-length b1))
                    (* (total-weight_ b2) (branch-length b2)))
                (if (pair? s1) (balanced? s1) true)
                (if (pair? s2) (balanced? s2) true)))))

; テスト
(define mobile1
    (make-mobile (make-branch 1 5) (make-branch 2 10)))
(define mobile2
    (make-mobile
        (make-branch 2 20)
        (make-branch
            5
            (make-mobile (make-branch 1 4) (make-branch 1 4)))))

(displayln (total-weight mobile1)) ; 15
(displayln (total-weight mobile2)) ; 28

(displayln (balanced? mobile1)) ; #f
(displayln (balanced? mobile2)) ; #t
