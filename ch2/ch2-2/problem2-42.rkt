#lang sicp
(#%require (only racket/base displayln))

; List
(define (filter pred? seq)
    (cond ((null? seq) nil)
          ((pred? (car seq))
            (cons (car seq) (filter pred? (cdr seq))))
          (else (filter pred? (cdr seq)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

; Property list
(define (lookup key seq)
    (car
        (filter
            (lambda (pair) (equal? (car pair) key))
            seq)))

(define (get-value key seq)
    (cdr (lookup key seq)))

; Eight queens

; type row = number
; type col = number
; type board = list (col * row) ; NOTE: 実装の都合上、列・行の順である点に注意

; row -> col -> board -> board
(define (adjoin-position row k positions)
    (cons (cons k row) positions))

; board
(define empty-board nil)

; col -> board -> boolean
(define (safe? k positions)
    (accumulate
        (lambda (l acc)
            (define safe-row
                (not (= (get-value k positions) (get-value l positions))))
            (define safe-diagonal
                (not (=
                    (abs (- (get-value k positions) (get-value l positions)))
                    (- k l))))
            (and safe-row safe-diagonal acc))
        true
        (enumerate-interval 1 (- k 1))))

; number -> list (board)
(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map
                            (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))

; テスト
(lookup 3 (list (cons 3 1) (cons 2 2))) ; (3 . 1)
(lookup 2 (list (cons 3 1) (cons 2 2))) ; (2 . 2)
(lookup 3 (list (cons 3 1) (cons 2 2) (cons 3 2))) ; (3 . 1)

(get-value 3 (list (cons 3 1) (cons 2 2))) ; 1
(get-value 2 (list (cons 3 1) (cons 2 2))) ; 2

(newline)

; cf. https://ja.wikipedia.org/wiki/%E3%82%A8%E3%82%A4%E3%83%88%E3%83%BB%E3%82%AF%E3%82%A4%E3%83%BC%E3%83%B3
(for-each
    (lambda (q)
        (displayln (length q)) ; 1, 0, 0, 2, 10, 4, 40, 92 の順に表示される
        (displayln q)
        (newline))
    (map queens (enumerate-interval 1 8)))
