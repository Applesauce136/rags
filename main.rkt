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
(define draw-pixels
  (lambda (gen)
    (let draw-pixel
        ((val (gen)))      
      (if val
          (begin
            (call-with-values (lambda () (apply values (map exact-floor val)))
              (lambda (x y z)
                ;; (printf "x: ~a    y: ~a~n" x y)
                (when (and (< -1 x width)
                           (< -1 y height))
                  (send my-bitmap-dc set-pixel
                        x y (make-color 255 255 0)))))
            (draw-pixel (gen)))
          #f))))

(when (terminal-port? (current-input-port))
  (display "Enter the name of your file: "))
(define-namespace-anchor commands-ns-anchor)
(define trash ; so that you don't print it
  (map (curryr eval (namespace-anchor->namespace commands-ns-anchor))
       (call-with-input-file (symbol->string (read))
         get-commands)))
