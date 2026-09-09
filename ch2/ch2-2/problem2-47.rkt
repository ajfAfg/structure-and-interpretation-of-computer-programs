#lang sicp
(#%require sicp-pict)

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cdr (cdr frame))))


(define (make-frame_ origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame_ frame) (car frame))
(define (edge1-frame_ frame) (car (cdr frame)))
(define (edge2-frame_ frame) (cdr (cdr frame)))
