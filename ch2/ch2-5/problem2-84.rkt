#lang sicp
(#%require (only racket/base displayln))

; 補助関数
(define (id x) x)
(define (square x) (* x x))
(define (find pred items)
    (cond ((null? items) false)
          ((pred (car items)) (car items))
          (else (find pred (cdr items)))))
(define (every pred items)
    (cond ((null? items) true)
          ((pred (car items)) (every pred (cdr items)))
          (else false)))
(define (any pred items)
    (cond ((null? items) false)
          ((pred (car items)) true)
          (else (any pred (cdr items)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; タグつきデータ
(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum: CONTENTS" datum)))

; データ主導プログラミングの道具
(define op-table '())

(define (put op type item)
    (set! op-table (cons (list (list op type) item) op-table)))

(define (get op type)
    (let ((record (assoc (list op type) op-table)))
        (if record (cadr record) false)))

(define coercion-table '())

(define (put-coercion type1 type2 item)
    (set! coercion-table (cons (list (list type1 type2) item) coercion-table)))

(define (get-coercion type1 type2)
    (let ((record (assoc (list type1 type2) coercion-table)))
        (if record (cadr record) false)))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (< 0 (length args))
                    (let ((to (accumulate join-types (car type-tags) (cdr type-tags)))
                          (mixed-types? (any (lambda (tag) (not (eq? tag (car type-tags)))) (cdr type-tags))))
                        (if (and to mixed-types?)
                            (apply apply-generic
                                   (cons op (map (lambda (x) (raise-to x to)) args)))
                            (error "No method for these types"
                                   (list op type-tags))))
                    (error "No method for these types"
                           (list op type-tags)))))))

; 型のタワー
(define tower-table '())

(define (put-raise type supertype coerce)
    (set! tower-table (cons (list type supertype coerce) tower-table)))

(define (get-supertype type)
    (let ((record (assoc type tower-table)))
        (if record (cadr record) false)))

(define (get-raise type)
    (let ((record (assoc type tower-table)))
        (if record (caddr record) false)))

(define (raise value) ((get-raise (type-tag value)) value))

(define (collect-upper-types type)
    (let ((supertype (get-supertype type)))
        (if supertype
            (cons type (collect-upper-types supertype))
            (list type))))

(define (join-types type1 type2)
    (let ((upper-types (collect-upper-types type1)))
        (find
            (lambda (t) (memq t upper-types))
            (collect-upper-types type2))))

(define (raise-to value type)
    (if (eq? (type-tag value) type)
        value
        (raise-to (raise value) type)))

; 四則演算
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; 整数
(define (install-integer-package)
    (define (tag x) (attach-tag 'integer x))
    (put 'add '(integer integer)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
        (lambda (x y) (tag (* x y))))
    ; 整数の div は 0 方向への切り捨て
    (put 'div '(integer integer)
        (lambda (x y) (tag (quotient x y))))
    (put 'make 'integer (lambda (x) (tag x)))

    ; 型のタワーに登録
    (put-raise 'integer 'rational
        (lambda (integer) ((get 'make 'rational) (contents integer) 1)))

    'done)
(define (make-integer n)
    ((get 'make 'integer) n))

; 実数
(define (install-real-package)
    (define (tag x) (attach-tag 'real x))
    (put 'add '(real real)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(real real)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(real real)
        (lambda (x y) (tag (* x y))))
    (put 'div '(real real)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'real (lambda (x) (tag (exact->inexact x))))

    ; 型のタワーに登録
    (put-raise 'real 'complex
        (lambda (real) ((get 'make-from-real-imag 'complex) (contents real) 0)))

    'done)
(define (make-real x)
    ((get 'make 'real) x))

; 有理数
(define (install-rational-package)
    ;; 内部手続き
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))

    ;; システムのほかの部分とのインタフェース
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))

    ; 型のタワーに登録
    (put-raise 'rational 'real
        (lambda (rational)
            (let ((x (contents rational)))
                ((get 'make 'real) (/ (numer x) (denom x))))))

    'done)
(define (make-rational n d)
    ((get 'make 'rational) n d))

; 複素数
(define (install-rectangular-package)
    ;; 内部手続き
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))

    ;; システムのほかの部分とのインタフェース
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
        (lambda (r a) (tag (make-from-mag-ang r a))))

    'done)

(define (install-polar-package)
    ;; 内部手続き
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))

    ;; システムのほかの部分とのインタフェース
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
        (lambda (r a) (tag (make-from-mag-ang r a))))

    'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
    ;; 直交形式パッケージと極形式パッケージからインポートした手続き
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;; 内部手続き
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))

    ;; システムのほかの部分とのインタフェース
    (define (tag z) (attach-tag 'complex z))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))

    'done)
(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))

; テスト
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-real-package)
(install-rational-package)
(install-integer-package)

(add (make-integer 1) (make-complex-from-real-imag 2 3)) ; (complex rectangular 3.0 . 3)
(add (make-rational 1 2) (make-integer 1)) ; (rational 3 . 2)

(define (add3 x y z) (apply-generic 'add3 x y z))
(put 'add3 '(real real real)
    (lambda (x y z) (make-real (+ x y z))))
(add3 (make-integer 1) (make-rational 1 2) (make-real 0.25)) ; (real . 1.75)

(apply-generic 'dummy
               (make-complex-from-real-imag 1 1)
               (make-complex-from-real-imag 2 2)) ; No method for these types
