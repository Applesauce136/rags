#lang racket

(require "draw.rkt"
         "display.rkt"
         "transform.rkt"
         "curves.rkt")

;; convenience function for testin

(define write-default
  (curry write-pixels
         600 300 '(255 255 255) "pic.ppm"))

;; parsing stuff
;; ----------------------------------------------------------------
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

;; ================================================================

;; stitching together of bits
;;----------------------------------------------------------------

(define shapes '())

(define transforms
  (curry identity))

(define push-shape
  (lambda (shape)
    (set! shapes
      (cons shape
            shapes))))

;; ================================================================

;; DW's commands
;;----------------------------------------------------------------

(define steps 5)

(define c
  (lambda (cx cy r)
    (push-shape (map (circle steps (list cx cy 0) r)
                            (build-list steps identity)))))

(define h
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-shape (map (hermite-curve steps
                                    (list x0 y0 0)
                                    (list x2 y2 0)
                                    (/ y1 x1)
                                    (/ y3 x3))
                     (build-list steps identity)))))

(define b
  (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
    (push-shape (map (bezier-curve steps
                                   (list x0 y0 0)
                                   (list x1 y1 0)
                                   (list x2 y2 0)
                                   (list x3 y3 0))
                     (build-list steps identity)))))

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

(define v
  (lambda ()
    ;; (write-default (append-map (curry draw-line '(255 255 255))
    ;;                            starts
    ;;                            ends))
    '()))

(define g
  (lambda (filename)
    ;; (map (lambda (shape) (printf "~a~n" shape))
    ;;      shapes)
    (write-pixels 500 500 '(255 255 255)
                  (if (symbol? filename)
                      (symbol->string filename)
                      filename)
                  (append-map (curry draw-lines '(0 0 0))
                              shapes))))
;; ================================================================

;; (display "Enter the filename of your script: ")
;; (read-script (symbol->string (read)))

(read-script "script_c")
