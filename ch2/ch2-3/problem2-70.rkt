#lang sicp
(#%require (only racket/base displayln))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

; 復号化
(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

; 暗号化
(define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((memq symbol (symbols (left-branch tree)))
            (cons 0 (encode-symbol symbol (left-branch tree))))
          ((memq symbol (symbols (right-branch tree)))
            (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "bad symbol: ENCODE-SYMBOL" symbol))))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

; ハフマン符号化木の生成
(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define (successive-merge trees) ; (< 0 (length trees)) と仮定
    (if (null? (cdr trees))
        (car trees)
        (successive-merge
            (adjoin-set
                (make-code-tree (car trees) (cadr trees))
                (cddr trees)))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

; テスト
(define tree
    (generate-huffman-tree
        '((A    2)
          (GET  2)
          (SHA  3)
          (WAH  1)
          (BOOM 1)
          (JOB  2)
          (NA  16)
          (YIP  9))))
(define message
    '(GET A JOB
      SHA NA NA NA NA NA NA NA NA
      GET A JOB
      SHA NA NA NA NA NA NA NA NA
      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
      SHA BOOM))

(length (encode message tree)) ; 84

; 固定長符号の場合に必要なビット数
(* 3 (length message)) ; 108、なお 8 つの記号を区別するためには高々 3 ビット必要
