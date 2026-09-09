#lang sicp
(#%require sicp-pict)
(#%require (only "save-painter.rkt" save-painter))

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(define (rotate180 painter)
    (transform-painter painter
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

(define (rotate270 painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(save-painter (flip-horiz einstein) "images/problem2-50-1.png")
(save-painter (rotate180 einstein) "images/problem2-50-2.png")
(save-painter (rotate270 einstein) "images/problem2-50-3.png")
