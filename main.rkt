#lang racket

(require "draw.rkt"
         "display.rkt"
         "transform.rkt")

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
         600 300 '(0 0 0) "pic.ppm"))

;; listxyzavg

(define starts
  '())

(define ends
  '())

(define transforms
  (curry identity))

(define pixels
  '())

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
    (write-default (append-map (curry draw-line '(255 255 255))
                               starts
                               ends))))

(define g
  (lambda (filename)
    (write-points 500 500 '(0 0 0)
                  (if (symbol? filename)
                      (symbol->string filename)
                      filename)
                  (append-map (curry draw-line '(255 255 255))
                              starts
                              ends))))

(define read-all
  (lambda (in proc)
    (define datum (read in))
    (unless (eq? datum eof)
      (read-all in
                (cond ((eq? proc (void))
                       ((curry (eval datum))))
                      ((procedure? ((curry proc datum)))
                       (curry proc datum))
                      (else
                       (void)))))))
