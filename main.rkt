#lang racket

(require racket/draw
         "commands.rkt"
         "compiler.rkt")

<<<<<<< Updated upstream
;; BITMAP
;; ----------------------------------------------------------------
;; (define width 500)
;; (define height 500)
(define my-bitmap-dc (new bitmap-dc% (bitmap
                                      (make-bitmap 500 500))))
=======
(define basename "")
(define varys (vector))
(define stack '())
(set! stack (cons identity stack))
(define width 500)
(define height 500)
(define my-bitmap-dc (new bitmap-dc% (bitmap (make-bitmap width height))))
>>>>>>> Stashed changes
(send my-bitmap-dc set-background (make-color 0 0 0))
(send my-bitmap-dc clear)
(define draw-pixels
  (lambda (gen)
<<<<<<< Updated upstream
    (call-with-values (lambda ()
                        (send my-bitmap-dc get-size))
      (lambda (width height)
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
              #f))))))
(define save
  (lambda (filename) 
    (send (send my-bitmap-dc get-bitmap)
          save-file filename 'png)))
;; ================================================================
=======
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
>>>>>>> Stashed changes

;; ANIMATION
;; ----------------------------------------------------------------
(define frames
  (lambda (num)
    '()))

(define basename
  (lambda (filename)
    '()))

(define vary
  (lambda (vary-name start end min max)
    '()))
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
  (lambda (x y z (fraction 1))
    (set! stack (cons (compose (apply move-point
                                      (map (curry * fraction)
                                           (list x y z)))
                               (first stack))
                      (rest stack)))))
(define scale
  (lambda (x y z (fraction 1))
    (set! stack (cons (compose (scale-point
                                (map (curry * fraction)
                                     (list x y z)))
                               (first stack))
                      (rest stack)))))

(define rotate
  (lambda (axis angle-d (fraction 1))
    (set! stack (cons (compose (rotate-point axis (* fraction
                                                     angle-d))
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
  (lambda (x y z radius-t radius-c)
    (draw-pixels
     (make-torus (first stack) x y z radius-t radius-c))))
(define line
  (lambda (x0 y0 z0 x1 y1 z1)
    (draw-pixels
     (make-line (first stack) x0 y0 z0 x1 y1 z1))))
;; ================================================================

;; MAIN
;; ----------------------------------------------------------------
(when (terminal-port? (current-input-port))
  (display "Enter the name of your file: "))

<<<<<<< Updated upstream
(define-namespace-anchor commands-ns-anchor)
(define ns (namespace-anchor->namespace commands-ns-anchor))

(define trash ; so that you don't print it
  (map (curryr eval ns)
       (call-with-input-file (symbol->string (read))
         get-commands)))
;; ================================================================
=======
(printf "Working...~n")

(define-namespace-anchor commands-ns-anchor)
(define ns (namespace-anchor->namespace
            commands-ns-anchor))
(call-with-input-file (symbol->string (read))
  (lambda (in)
    (define commands (get-commands in))
    (let one-command ((command (commands)))
      (if command
          (begin (eval command ns)
                 (one-command (commands)))
          (printf "Done!~n")))))
>>>>>>> Stashed changes
