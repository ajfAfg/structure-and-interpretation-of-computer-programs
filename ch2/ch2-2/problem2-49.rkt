#lang sicp
(#%require sicp-pict)
(#%require (only "save-painter.rkt" save-painter))

(define a-painter
    (segments->painter
        (list
            (make-segment (make-vect 0 0) (make-vect 0 1))
            (make-segment (make-vect 0 1) (make-vect 1 1))
            (make-segment (make-vect 1 1) (make-vect 1 0))
            (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define b-painter
    (segments->painter
        (list
            (make-segment (make-vect 0 0) (make-vect 1 1))
            (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define c-painter
    (segments->painter
        (list
            (make-segment (make-vect   0  0.5) (make-vect 0.5    1))
            (make-segment (make-vect 0.5    1) (make-vect   1  0.5))
            (make-segment (make-vect   1  0.5) (make-vect 0.5    0))
            (make-segment (make-vect 0.5    0) (make-vect   0  0.5)))))

(save-painter a-painter "images/problem2-49-a.png")
(save-painter b-painter "images/problem2-49-b.png")
(save-painter c-painter "images/problem2-49-c.png")

; d はお絵描き問題なので割愛
