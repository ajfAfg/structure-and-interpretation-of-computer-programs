#lang sicp
(#%require (only racket/base displayln))

(define (reverse items)
    (define (iter items acc)
        (if (null? items)
            acc
            (iter (cdr items) (cons (car items) acc))))
    (iter items nil))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values) (null? coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+
                (cc
                    amount
                    (except-first-denomination coin-values))
                (cc
                    (- amount (first-denomination coin-values))
                    coin-values)))))

; テスト
(displayln (cc 100 us-coins)) ; 292
(displayln (cc 100 (reverse us-coins))) ; 292
