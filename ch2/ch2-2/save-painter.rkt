#lang racket/base
;; ペインタを PNG ファイルに書き出す。
;; #lang sicp のファイルからは (#%require "save-painter.rkt") で取り込む。
(require racket/class racket/file sicp-pict)
(provide save-painter)

;; save-painter : painter path [width] [height] -> void
(define (save-painter painter path [width 200] [height 200])
  (define-values (dir _name _must-be-dir?)
    (split-path (path->complete-path path)))
  (when (path? dir)
    (make-directory* dir))
  (define snip (paint painter #:width width #:height height))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file path 'png)
  (displayln (string-append "saved: " path)))
