#lang racket

(require "base/main.rkt"
         "draw/main.rkt"
         racket/draw)

(provide (all-from-out
          "base/main.rkt"
          "draw/main.rkt"))

;; stitching together of bits
;;----------------------------------------------------------------

(define shapes '())

(define steps 100)

(define push-shape
  (lambda (shape)
    (set! shapes
      (cons shape
            shapes))))

(define transforms
  (curry identity))

;; ================================================================

;; DW's commands
;;----------------------------------------------------------------

(define p
  (lambda (x y z width height depth)
    (push-shape (box (list x y z) width height depth))))

(define m
  (lambda (x y radius)
    (push-shape (sphere steps (list x y 0) radius))))

(define d
  (lambda (x y rad-t rad-c)
    (push-shape (torus steps (list x y 0) rad-t rad-c))))

(define c
  (lambda (cx cy r)
    (push-shape (circle steps (list cx cy 0) r))))

(define h
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-shape (hermite-curve steps
                               (list x0 y0 0)
                               (list x2 y2 0)
                               (/ y1 x1)
                               (/ y3 x3)))))

(define b
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-shape (bezier-curve steps
                              (list x0 y0 0)
                              (list x1 y1 0)
                              (list x2 y2 0)
                              (list x3 y3 0)))))

(define l
  (lambda (xa ya za xb yb zb)
    (push-shape `((,xa ,ya ,za)
                  (,xb ,yb ,zb)))))

(define i
  (lambda ()
    (set! transforms (curry identity))))

(define w
  (lambda ()
    (set! shapes '())))

(define s
  (lambda (sx sy sz)
    (set! transforms
      (compose (curry scale sx sy sz)
               transforms))))

(define t
  (lambda (tx ty tz)
    (set! transforms
      (compose (curry translate tx ty tz)
               transforms))))

(define x
  (lambda (angle)
    (set! transforms
      (compose (curry rotate 'x angle)
               transforms))))

(define y
  (lambda (angle)
    (set! transforms
      (compose (curry rotate 'y angle)
               transforms))))

(define z
  (lambda (angle)
    (set! transforms
      (compose (curry rotate 'z angle)
               transforms))))

(define a
  (lambda ()
    (set! shapes (map transforms shapes))))

(define g
  (lambda (filename)
    ;; (map (lambda (shape)
    ;;        (map (lambda (pt)
    ;;               (map (lambda (num)
    ;;                      (printf "~a " (floor num)))
    ;;                    pt)
    ;;               (printf "~n"))
    ;;             shape)
    ;;        (printf "~n")) 
    ;;      shapes)
    (write-pixels 500 500 '(255 255 255)
                  (if (symbol? filename)
                      (symbol->string filename)
                      filename)
                  (append-map (curry draw-lines '(0 0 0))
                              shapes))))

(define v g)

;; ================================================================

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

;; (display "Enter the filename of your script: ")
;; (read-script (symbol->string (read)) ns)

(define target (make-bitmap 30 30))
(define dc (new bitmap-dc% (bitmap target)))
(send dc draw-rectangle 10 10 10 10)
(send target save-file "box.png" 'png)
