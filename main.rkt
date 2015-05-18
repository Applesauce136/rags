#lang racket

(require racket/draw
         "commands.rkt"
         "compiler.rkt")

;; BITMAP
;; ----------------------------------------------------------------
(define width 500)
(define height 500)
(define my-bitmap-dc (new bitmap-dc% (bitmap
                                      (make-bitmap width height))))
(send my-bitmap-dc set-background (make-color 0 0 0))
(send my-bitmap-dc clear)
(define draw-pixels
  (lambda (gen)
    (let draw-pixel
        ((val (gen)))      
      (if val
          (begin
            (call-with-values (lambda ()
                                (apply values
                                       (map exact-floor val)))
              (lambda (x y z)
                ;; (printf "x: ~a    y: ~a~n" x y)
                (when (and (< -1 x width)
                           (< -1 y height))
                  (send my-bitmap-dc set-pixel
                        x y (make-color 255 255 0)))))
            (draw-pixel (gen)))
          #f))))
(define save
  (lambda (filename) 
    (send (send my-bitmap-dc get-bitmap)
          save-file filename 'png)))
;; ================================================================

;; TRANSFORMATIONS
;; ----------------------------------------------------------------
(define stack (list identity))

(define push
  (lambda ()
    (set! stack
      (cons (first stack)
            stack))))

(define pop
  (lambda ()
    (set! stack (rest stack))))

(define move
  (lambda (x y z)
    (set! stack (cons (compose (move-point x y z)
                               (first stack))
                      (rest stack)))))
(define scale
  (lambda (x y z)
    (set! stack (cons (compose (scale-point x y z)
                               (first stack))
                      (rest stack)))))

(define rotate
  (lambda (axis angle_d)
    (set! stack (cons (compose (rotate-point axis angle_d)
                               (first stack))
                      (rest stack)))))
;; ================================================================

;; SHAPES
;; ----------------------------------------------------------------
(define box
  (lambda (x y z width height depth)
    (draw-pixels
     (make-box (first stack) x y z width height depth))))
(define sphere
  (lambda (x y z radius)
    (draw-pixels
     (make-sphere (first stack) x y z radius))))
(define torus
  (lambda (x y z radius_t radius_c)
    (draw-pixels
     (make-torus (first stack) x y z radius_t radius_c))))
(define line
  (lambda (x0 y0 z0 x1 y1 z1)
    (draw-pixels
     (make-line (first stack) x0 y0 z0 x1 y1 z1))))
;; ================================================================

;; MAIN
;; ----------------------------------------------------------------
(when (terminal-port? (current-input-port))
  (display "Enter the name of your file: "))
(define-namespace-anchor commands-ns-anchor)
(define trash ; so that you don't print it
  (map (curryr eval (namespace-anchor->namespace commands-ns-anchor))
       (call-with-input-file (symbol->string (read))
         get-commands)))
;; ================================================================
