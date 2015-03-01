#lang racket

(require "draw.rkt"
         "display.rkt"
         "matrix.rkt")

(provide
 add-line

 t-reset
 t-scale
 t-rotate
 transform

 draw
 write-pts!
 write-matrix!
 )

(define i-reset
  (lambda ()
    (set! img (make-image 400 200))))

(define add-line
  (lambda (x0 y0 z0 x1 y1 z1)
    (add-matrix-col! smtx `(,x0 ,y0 ,z0 1))
    (add-matrix-col! emtx `(,x1 ,y1 ,z1 1))))

(define p-reset
  (lambda ()
    (set! smtx (make-matrix 4))
    (set! emtx (make-matrix 4))))

(define t-reset
  (lambda ()
    (set! tmtx (make-matrix 4))
    (add-matrix-cols! tmtx
                 '(1 0 0 0) 
                 '(0 1 0 0)
                 '(0 0 1 0)
                 '(0 0 0 1))))

(define t-scale
  (lambda (tmtx sx sy sz)
    '()))

(define t-translate
  (lambda (tmtx tx ty tz)
    '()))

(define t-rotate
  (lambda (tmtx axis angle)
    '()))

(define transform
  (lambda (tmtx mtx)
    '()))

(define reset-img!
  (lambda (rows cols)
    (set! img (make-image rows cols))))

(define set-img-name!
  (lambda (str)
    (set! img-name str)))

(define draw
  (lambda ()
    (append-map draw-line 
                (list-ref (matrix-cols smtx) 0)
                (list-ref (matrix-cols smtx) 1)
                (list-ref (matrix-cols emtx) 0)
                (list-ref (matrix-cols emtx) 1))))

(define write-pts!
  (lambda (img pts pix)
    (map (lambda (pt)
           (set-image-pixel! img pix 
                             (list-ref pt 0)
                             (list-ref pt 1)))
         pts)))

(define write-matrix!
  (lambda ((pix (make-pixel 255 255 255)))
    (write-pts! img (draw) pix)
    (write-image! img img-name)))

(define smtx (make-matrix 4))
(define emtx (make-matrix 4))
(define tmtx (make-matrix 4))
(define img (make-image 400 200))
(define img-name "pic.ppm")
(t-reset)
