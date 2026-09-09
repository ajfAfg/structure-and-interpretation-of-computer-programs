#lang sicp
(#%require sicp-pict)
(#%require (only "save-painter.rkt" save-painter))

(define (split big-combiner small-combiner)
    (define (iter painter n)
        (if (= n 0)
            painter
            (let ((smaller (iter painter (- n 1))))
                (big-combiner painter (small-combiner smaller smaller)))))
    iter)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                  (bottom-right (below right right))
                  (corner (corner-split painter (- n 1))))
                (beside (below painter top-left)
                        (below bottom-right corner))))))

; テスト
(save-painter (corner-split einstein 4) "images/problem2-45.png")
