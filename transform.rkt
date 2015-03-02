#lang racket

(require "contracts.rkt")

(provide
 translate
 scale
 rotate
 )

(define translate
  (lambda (pts tx ty tz)
    (map
     (lambda (pt)
       (list (+ tx (first pt))
             (+ ty (second pt))
             (+ tz (third pt))))
     pts)))

(define scale
  (lambda (pts sx sy sz)
    (map
     (lambda (pt)
       (list (* sx (first pt))
             (* sy (second pt))
             (* sz (third pt))))
     pts)))

(define rotate
  (lambda (pts axis angle)
    ;; (define axis_num (cond ((eq? 'x axis) 0)
    ;;                        ((eq? 'y axis) 1)
    ;;                        ((eq? 'z axis) 2)))
    (map
     (lambda (pt)
       (let ((x (first pt))
             (y (second pt))
             (z (third pt)))
         (cond ((eq? 'x axis)
                (list x
                      y
                      z))
               ((eq? 'y axis)
                )
               ((eq? 'z axis)
                ))))
     pts)))
