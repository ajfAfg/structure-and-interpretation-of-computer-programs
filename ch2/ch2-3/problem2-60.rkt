#lang sicp
(#%require (only racket/base displayln))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
            (cons (car set1) (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) (append set1 set2))

; テスト
(intersection-set '(1 2 3 3) '(3 4 5 3)) ; (3 3)
(intersection-set '(1 2 3) '()) ; ()

(union-set '(1 2 3) '(3 4 5)) ; (1 2 3 3 4 5)
(union-set '(1 2 3) '()) ; (1 2 3)

; 計算量の違い:
;
; |                  | 重複なしリスト   | 重複ありリスト   |
; | :--------------: | :--------------: | :--------------: |
; | element-of-set?  | O(N)             | O(N)             |
; | adjoin-set       | O(N)             | O(1)             |
; | intersection-set | O(N^2)           | O(N^2)           |
; | union-set        | O(N^2)           | O(N)             |

; 重複ありリストが嬉しい場合は、
; 少なくとも上 4 つの演算、特に adjoin-set と union-set を多用する場合かなぁ。
