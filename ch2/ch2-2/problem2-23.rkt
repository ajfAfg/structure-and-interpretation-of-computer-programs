#lang sicp
(#%require (only racket/base displayln))

(define (for-each proc items)
    (map proc items)
    #t)

; テスト
(for-each
    (lambda (x) (newline) (display x))
    (list 57 321 88))
