#lang racket

(require "contracts.rkt")

(provide
 pixs->image
 image->string
)

(define pixs->image
  (lambda (rows cols color pixs)
    (define img (make-vector (+ 1 (* rows cols)) color))
    (vector-set! img 0 (list rows cols 255))
    (map (lambda (pix)
           (define row (first pix))
           (define col (second pix))
           (define color (third pix))
           (vector-set! img 
                        (+ 1
                           (* row rows)
                           col)
                        color))
         pixs)
    img))

(define image->string
  (lambda (img)
    (string-join #:before-first "P3 "
     (vector->list 
      (vector-map (lambda (color)
                    (string-join (map number->string color)))
                  img)))))
