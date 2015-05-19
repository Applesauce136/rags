#lang racket

(require racket/draw
         racket/generator
         
         "commands.rkt"
         "compiler.rkt"
         "format.rkt")

;; BITMAP
;; ----------------------------------------------------------------
(define my-bitmap-dc (new bitmap-dc% (bitmap
                                      (make-bitmap 500 500))))
(send my-bitmap-dc set-background (make-color 0 0 0))
(define draw-pixels
  (lambda (gen)
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

;; ANIMATION
;; ----------------------------------------------------------------
(define vary
  (lambda (vary-name start end min max)
    (do ((index start (+ index 1))
         (value min (* (/ (- index start)
                          (- end start))
                       (- max min))))
        ((= index end) (hash-set! (vector-ref knobs index)
                                  vary-name value))
      (hash-set! (vector-ref knobs index)
                 vary-name value))))
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
(define reset
  (lambda ()
    (send my-bitmap-dc clear)
    (set! stack (list identity))))

(when (terminal-port? (current-input-port))
  (display "Enter the name of your file: "))

(define-namespace-anchor commands-ns-anchor)
(define ns (namespace-anchor->namespace commands-ns-anchor))
(define my-eval (curryr eval ns))

(define raw-commands (call-with-input-file (symbol->string (read))
       get-commands))

(define frames (filter (lambda (lst)
                         (eq? (car lst) 'frames))
                       raw-commands))
(define basename (filter (lambda (lst)
                           (eq? (car lst) 'basename))
                         raw-commands))
(define varys (filter (lambda (lst)
                        (eq? (car lst) 'vary))
                      raw-commands))
(define loop-commands (filter (lambda (lst)
                                (memq (car lst)
                                      '(push pop
                                             move scale rotate
                                             box sphere torus line)))
                              raw-commands))

(define knobs (for/vector #:length (if (null? frames)
                                       0
                                       (cadar frames))
                          ((frame (in-generator
                                   (let loop ((index 0))
                                     (yield index)
                                     (loop (+ index 1))))))
                          (make-hasheq `((frame ,frame)))))

(reset)
(make-directory (cadar basename))
(current-directory (cadar basename))
(if (or (null? frames)
        (null? basename)
        (null? varys))
    (map my-eval
         (filter-not (lambda (lst)
                       (memq (car lst)
                             '(vary basename frames)))
                     raw-commands))
    (begin
      (map my-eval varys)
      (for ((knob knobs))
        (for ((command loop-commands))
          (if (and (memq (car command) '(move scale rotate))
                   (symbol? (my-eval (last command))))
              (when (hash-has-key? knob (my-eval (last command)))
                (my-eval (append (drop-right command 1)
                                 (list (hash-ref knob (my-eval (last command)))))))
              (my-eval command)))
        (save (string-append
               (cadar basename) "_"
               (pad (car (hash-ref knob 'frame)) (number-length (cadar frames)))
               ".png"))
        (reset))))
;; ================================================================
