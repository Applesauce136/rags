#lang racket

(require "base/main.rkt"
         "draw/main.rkt")

(provide (all-from-out
          "base/main.rkt"
          "draw/main.rkt"))

;; stitching together of bits
;;----------------------------------------------------------------

(define shapes '())

(define push-shape
  (lambda (shape)
    (set! shapes
      (cons shape
            shapes))))

(define steps 100)

(define shapify
  (lambda (proc steps #:cyclic (cycle? #f))
    (map proc
         (append (build-list steps (curryr / steps))
                 (if cycle?
                     '(0)
                     '())))))

(define transforms
  (curry identity))

;; ================================================================

;; DW's commands
;;----------------------------------------------------------------

(define c
  (lambda (cx cy r)
    (push-shape (shapify (circle (list cx cy 0) r)
                         steps
                         #:cyclic #t))))

(define h
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-shape (shapify (hermite-curve (list x0 y0 0)
                                        (list x2 y2 0)
                                        (/ y1 x1)
                                        (/ y3 x3))
                         steps))))

(define b
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-shape (shapify (bezier-curve (list x0 y0 0)
                                       (list x1 y1 0)
                                       (list x2 y2 0)
                                       (list x3 y3 0))
                         steps))))

(define l
  (lambda (xa ya za xb yb zb)
    (push-shape `((,xa ,ya ,za)
                  (,xb ,yb ,zb)))))

(define i
  (lambda ()
    (set! transforms (curry identity))))

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

;; (display "Enter the filename of your script: ")
;; (read-script (symbol->string (read)))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(read-script "script_c" ns)
