#lang racket

(require racket/draw
         "commands.rkt"
         "compiler.rkt")

;; BITMAP
;; ----------------------------------------------------------------
;; (define width 500)
;; (define height 500)
(define my-bitmap-dc (new bitmap-dc% (bitmap
                                      (make-bitmap 500 500))))
(send my-bitmap-dc set-background (make-color 0 0 0))
(send my-bitmap-dc clear)
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

;; ANIMATION
;; ----------------------------------------------------------------
(define vary
  (lambda (vary-name start end min max)
    '(do ((index ,$3 (+ index 1))
          (value ,$5 (+ value (* (/ (- index ,$3)
                                    (- ,$4 ,$3))
                                 ,$6))))
         ((= index ,$4) (hash-set! (vector-ref varys index)
                                   (string->symbol ,$2) value))
       (hash-set! (vector-ref varys index)
                  (string->symbol ,$2) value))))
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

(define-namespace-anchor commands-ns-anchor)
(define ns (namespace-anchor->namespace commands-ns-anchor))

;; (map (curryr eval ns)
;;      (call-with-input-file (symbol->string (read))
;;        get-commands))

(define commands (call-with-input-file (symbol->string (read))
       get-commands))

(define frames (cadar ; the second element of the first match
                (filter (lambda (lst)
                          (eq? (car lst) 'frames))
                        commands)))
(define basename (cadar ; same thing
                  (filter (lambda (lst)
                            (eq? (car lst) 'basename))
                          commands)))
(define varys (filter (lambda (lst)
                        (eq? (car lst) 'vary))
                      commands))

(printf "FRAMES: ~a~n" frames)
(printf "BASENAME: ~a~n" basename)
(printf "VARYS: ~a~n" varys)

(map (curry printf "~a~n") commands)
;; ================================================================
