#lang racket

(require racket/draw
         "commands.rkt"
         "compiler.rkt")

(define stack '())
(set! stack (cons identity stack))
(define width 500)
(define height 500)
(define my-bitmap-dc (new bitmap-dc% (bitmap (make-bitmap width height))))
(send my-bitmap-dc set-background (make-color 0 0 0))
(send my-bitmap-dc clear)
(define draw-pixel
  (lambda (pixel)
    (when (and (< -1 (first pixel) width)
               (< -1 (second pixel) height))
      (send my-bitmap-dc set-pixel
            (exact-floor (first pixel))
            (exact-floor (second pixel))
            (make-color 255 255 0)))))

(display "Enter the name of your file: ")
(define-namespace-anchor commands-ns-anchor)
(define trash ; so that you don't print it
  (map (curryr eval (namespace-anchor->namespace commands-ns-anchor))
       (call-with-input-file (symbol->string (read))
         get-commands)))
