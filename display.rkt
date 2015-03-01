#lang racket

(define pts->image
  (lambda (rows cols color pts)
    (define img (make-vector (+ 1 (* rows cols)) color))
    (vector-set! img 0 (list rows cols 255))
    (map (lambda (pt)
           (define row (first pt))
           (define col (second pt))
           (define color (fourth pt))
           (vector-set! img 
                        (+ (* row rows)
                           col
                           1)
                        color))
         pts)
    img))

(define image->string
  (lambda (img)
    (string-join #:before-first "P3 "
     (vector->list 
      (vector-map (lambda (color)
                    (string-join (map number->string color)))
                  img)))))
