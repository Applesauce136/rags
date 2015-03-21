#lang racket

(require "draw.rkt"
         "display.rkt"
         "transform.rkt"
         "curves.rkt")

(define write-points
  (lambda (rows cols bg filename pts)
    (call-with-output-file filename
      #:exists 'replace
      (lambda (out)
        (display
         (image->string
          (pixels->image rows cols bg
                         pts))
         out)))))

(define write-default
  (curry write-points
         500 500 '(255 255 255) "pic.ppm"))

;; constants

(define starts
  '())

(define ends
  '())

(define transforms
  (curry identity))

(define pixels
  '())

(define steps 20)

;; functions (chblistxyzavg)

(define c
  (lambda (cx cy r)
    (set! starts (append (map (circle steps (list cx cy 0) r)
                              (build-list steps identity))
                         starts))
    (set! ends (append (map (circle steps (list cx cy 0) r)
                            (map add1 (build-list steps identity)))
                       ends))))

(define h
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (set! starts (append (map (hermite-curve steps
                                             (list x0 y0 0)
                                             (/ y1 x1)
                                             (list x2 y2 0)
                                             (/ y3 x3))
                              (build-list steps identity))
                         starts))
    (set! ends (append (map (hermite-curve steps
                                             (list x0 y0 0)
                                             (/ y1 x1)
                                             (list x2 y2 0)
                                             (/ y3 x3))
                            (map add1 (build-list steps identity)))
                         ends))))

(define b
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (set! starts (append (map (bezier-curve steps
                                            (list x0 y0 0)
                                            (list x1 y1 0)
                                            (list x2 y2 0)
                                            (list x3 y3 0))
                              (build-list steps identity))
                         starts))
    (set! ends (append (map (bezier-curve steps
                                          (list x0 y0 0)
                                          (list x1 y1 0)
                                          (list x2 y2 0)
                                          (list x3 y3 0))
                            (map add1 (build-list steps identity)))
                       ends))))

(define l
  (lambda (xa ya za xb yb zb)
    (set! starts (cons (list xa ya za) starts))
    (set! ends (cons (list xb yb zb) ends))))

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
    (set! starts (transforms starts))
    (set! ends (transforms ends))))

(define v
  (lambda ()
    (write-default (append-map (curry draw-line '(0 0 0))
                               starts
                               ends))))

(define g
  ;; (lambda (filename)
  ;;   (write-points 500 500 '(255 255 255)
  ;;                 (if (symbol? filename)
  ;;                     (symbol->string filename)
  ;;                     filename)
  ;;                 (append-map (curry draw-line '(255 255 255))
  ;;                             starts
  ;;                             ends)))
  v)

(define read-script
  (lambda (filename)
    (call-with-input-file filename
      (lambda (in)
        (read-script-helper in (void))))))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define read-script-helper
  (lambda (in proc)
    (define datum (read in))
    (unless (eof-object? datum)
      (read-script-helper
       in
       (cond ((eq? proc (void))
              ((curry (eval datum ns))))
             ((procedure? ((curry proc datum)))
              (curry proc datum))
             (else
              (void)))))))

(display "Enter the filename of your script: ")
(read-script (symbol->string (read)))
