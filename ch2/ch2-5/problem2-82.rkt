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
(define (head-or items default)
    (if (= (length items) 0)
        default
        (car items)))

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
                (let ((coercions (find (lambda (items) (every id items))
                                 (map
                                    (lambda (type1)
                                        (map
                                            (lambda (type2)
                                                (if (eq? type1 type2)
                                                    id
                                                    (get-coercion type2 type1)))
                                            type-tags))
                                    type-tags)))
                      (mixed-types? (any (lambda (tag) (not (eq? tag (car type-tags)))) (cdr type-tags))))
                    (if (and coercions mixed-types?)
                        (apply apply-generic
                               (cons op
                                     (map (lambda (coerce t) (coerce t))
                                          coercions
                                          args)))
                        (error "No method for these types"
                               (list op type-tags))))))))

; 四則演算
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; 通常の数値
(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number (lambda (x) (tag x)))

    'done)
(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

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
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (add3 x y z) (apply-generic 'add3 x y z))
(put 'add3 '(complex complex complex)
    (lambda (z1 z2 z3) (add (add (attach-tag 'complex z1)
                                 (attach-tag 'complex z2))
                            (attach-tag 'complex z3))))

(add (make-scheme-number 1) (make-complex-from-real-imag 2 3)) ; (complex rectangular 3 . 3)
(add (make-complex-from-real-imag 2 3) (make-scheme-number 1)) ; (complex rectangular 3 . 3)

(add3 (make-scheme-number 1)
      (make-complex-from-real-imag 2 3)
      (make-scheme-number 4)) ; (complex rectangular 7 . 3)

(add3 (make-scheme-number 1)
      (make-scheme-number 2)
      (make-scheme-number 3)) ; 無限ループせず異常終了する

; なお、今回の強制型変換の戦略だと、例えば全ての型をあるスーパータイプに変換すればよい場合に
; 手続きを見つけられない。
