#lang racket

(require "draw-primitives.rkt")
(provide make-drawable
         line%)

(define drawable<%>
  (interface ()
    pointify
    set-index
    set-point
    get-points))

(define make-drawable
  (lambda (draw-proc)
    (class* object% (drawable<%>)
      (super-new)
      (init (anchor '(0 0 0)))
      (define draw draw-proc)
      
      (define inner-anchor anchor)
      (define pts (make-vector (procedure-arity draw) '(0 0 0)))
      (define cur-index 0)

      (define/public pointify
        (lambda ()
          (apply draw (vector->list pts))))
      (define/public set-index
        (lambda (index)
          (when (< index (vector-length pts))
            (set! cur-index index))))
      (define/public inc-index
        (lambda ()
          (set-index (remainder (+ cur-index 1)
                                (vector-length pts)))))
      (define/public set-point
        (lambda (pt)
          (vector-set! pts cur-index pt)))
      (define/public get-points
        (lambda ()
          (vector->list pts))))))

(define line%
  (make-drawable draw-line))
