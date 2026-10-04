#lang sicp
(#%require (only racket/base displayln))

; レコード
(define (make-record key value) (cons key value))
(define (key record) (car record))

; 二分木
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
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

(define (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
           (else
            (let ((e (entry set-of-records)))
                (cond ((< given-key (key e)) (lookup given-key (left-branch set-of-records)))
                      ((> given-key (key e)) (lookup given-key (right-branch set-of-records)))
                      (else e))))))

; テスト
(define t
    (list->tree
        (list (make-record 1 'foo) (make-record 3 'bar) (make-record 5 'baz))))

(lookup 3 t) ; (3 . bar)
(lookup 2 t) ; #f
