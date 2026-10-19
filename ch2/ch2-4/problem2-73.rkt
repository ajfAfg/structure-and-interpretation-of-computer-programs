#lang sicp
(#%require (only racket/base displayln))

; データ主導プログラミングの道具
(define op-table '())

(define (put op type item)
    (set! op-table (cons (list (list op type) item) op-table)))

(define (get op type)
    (let ((record (assoc (list op type) op-table)))
        (if record (cadr record) false)))

; 代数式
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
(define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))

; 微分
(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp))
                 (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
    ;; 内部手続き
    (define (addend s) (car s))
    (define (augend s) (cadr s))
    (define (multiplier p) (car p))
    (define (multiplicand p) (cadr p))

    ;; システムの他の部分とのインタフェース
    (put 'deriv '+
        (lambda (operands var)
            (make-sum (deriv (addend operands) var)
                      (deriv (augend operands) var))))
    (put 'deriv '*
        (lambda (operands var)
            (make-sum
                (make-product (multiplier operands)
                              (deriv (multiplicand operands) var))
                (make-product (deriv (multiplier operands) var)
                              (multiplicand operands)))))

    'done)

(define (install-deriv-package2)
    ;; 内部手続き
    (define (base e) (car e))
    (define (exponent e) (cadr e))

    ;; システムの他の部分とのインタフェース
    (put 'deriv '**
        (lambda (operands var)
            (make-product (exponent operands)
                          (make-product
                            (make-exponentiation
                                (base operands)
                                (make-sum (exponent operands) -1))
                            (deriv (base operands) var)))))
    'done)

; テスト
; a.
; `number?` な場合、`variable?` な場合、それ以外の場合で場合分けして
; 記号微分を定義している。
; なお、`number?` と `variable?` なデータは
; `operator` と `operands` で中身を取り出せないため、
; データ主導ディスパッチとして取り込めない。

; b
(install-deriv-package)
(deriv (make-sum 'x 2) 'x) ; 1
(deriv (make-product 'x 2) 'x) ; 2

; c
(install-deriv-package2)
(deriv (make-exponentiation 'x 2) 'x) ; (* 2 x)

; d
; 加えるべき変更は、`put` 呼び出し時の引数の順序のみ。
