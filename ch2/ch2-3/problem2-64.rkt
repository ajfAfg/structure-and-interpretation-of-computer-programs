#lang sicp
(#%require (only racket/base displayln))

(define (make-tree entry left right)
    (list entry left right))

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elts))
                          (right-result (partial-tree (cdr non-left-elts) right-size)))
                        (let ((right-tree (car right-result))
                              (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                  remaining-elts))))))))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

; テスト
; (a)
; `partial-tree` は、最終的に作成する木のうち、最も「左」の木から作成する。
; 「左」の木の作成に用いなかった要素は上へ持ち上げられ、
; 「右」の木の作成に用いられる。
; なお、持ち上げられたリストの先頭要素はその木のエントリとなる。
(list->tree '(1 3 5 7 9 11)) ; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; (b)
; O(N)
