#lang racket

(require "base/main.rkt"
         "draw/main.rkt")

(provide (all-from-out
          "base/main.rkt"
          "draw/main.rkt"))

(define steps 20)

(define transforms
  (curry identity))

;; ================================================================

;; 3D things
;;----------------------------------------------------------------
;; --------------------------------

(define spatials '())

(define push-spatial
  (lambda (spatial)
    (set! spatials
      (cons spatial
            spatials))))

;; ================================

(define p
  (lambda (x y z width height depth)
    (push-spatial (box (list x y z) width height depth))))

(define m
  (lambda (x y radius)
    (push-spatial (sphere steps (list x y 0) radius))))

(define d
  (lambda (x y rad-t rad-c)
    (push-spatial (torus steps (list x y 0) rad-t rad-c))))

;; ================================================================

;; 2D things
;; ----------------------------------------------------------------
;; --------------------------------

(define planars '())

(define push-planar
  (lambda (planar)
    (set! planars
      (cons planar
            planars))))

;; ================================

(define c
  (lambda (cx cy r)
    (push-planar (circle steps (list cx cy 0) r))))

(define h
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-planar (hermite-curve steps
                               (list x0 y0 0)
                               (list x2 y2 0)
                               (/ y1 x1)
                               (/ y3 x3)))))

(define b
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-planar (bezier-curve steps
                              (list x0 y0 0)
                              (list x1 y1 0)
                              (list x2 y2 0)
                              (list x3 y3 0)))))

(define l
  (lambda (xa ya za xb yb zb)
    (push-planar `((,xa ,ya ,za)
                  (,xb ,yb ,zb)))))

;; ================================================================

;; drawin things
;; ----------------------------------------------------------------

(define i
  (lambda ()
    (set! transforms (curry identity))))

(define w
  (lambda ()
    (set! planars '())
    (set! spatials '())))

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
    (set! planars (map transforms planars))
    (set! spatials (map (curry map transforms) spatials))))

(define g
  (lambda (filename)
    ;; (map (lambda (planar)
    ;;        (map (lambda (pt)
    ;;               (map (lambda (num)
    ;;                      (printf "~a " (floor num)))
    ;;                    pt)
    ;;               (printf "~n"))
    ;;             planar)
    ;;        (printf "~n")) 
    ;;      planars)
    (write-pixels 500 500 '(255 255 255)
                  (if (symbol? filename)
                      (symbol->string filename)
                      filename)
                  (append (append-map (curry draw-lines '(0 0 0))
                                      planars)
                          (append-map (curry draw-triangles '(0 0 0))
                                      spatials)))))
;; EDIT THIS LATER

(define v g)

;; ================================================================

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(display "Enter the filename of your script: ")
(read-script (symbol->string (read)) ns)
