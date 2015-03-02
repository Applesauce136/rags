#lang racket

(require "contracts.rkt")

(provide (contract-out
          (translate
           (-> real? real? real? (listof point/c)
               (listof point/c)))
          (scale
           (-> real? real? real? (listof point/c)
               (listof point/c)))
          (rotate
           (-> symbol? real? (listof point/c)
               (listof point/c))))
 )

(define translate
  (lambda (tx ty tz pts)
    (map
     (lambda (pt)
       (list (+ tx (first pt))
             (+ ty (second pt))
             (+ tz (third pt))))
     pts)))

(define scale
  (lambda (sx sy sz pts)
    (map
     (lambda (pt)
       (list (* sx (first pt))
             (* sy (second pt))
             (* sz (third pt))))
     pts)))

(define rotate
  (lambda (axis angle_d pts)
    ;; (define axis_num (cond ((eq? 'x axis) 0)
    ;;                        ((eq? 'y axis) 1)
    ;;                        ((eq? 'z axis) 2)))
    (define angle (degrees->radians angle_d))
    (map
     (lambda (pt)
       (let ((x (first pt))
             (y (second pt))
             (z (third pt)))
         (cond ((eq? 'x axis)
                (list x
                      (- (* y (cos angle))
                         (* z (sin angle)))
                      (+ (* y (sin angle))
                         (* z (cos angle)))))
               ((eq? 'y axis)
                (list (+ (* x (sin angle))
                         (* z (cos angle)))
                      y
                      (- (* x (cos angle))
                         (* z (sin angle)))))
               ((eq? 'z axis)
                (list (- (* x (cos angle))
                         (* y (sin angle)))
                      (+ (* x (sin angle))
                         (* y (cos angle)))
                      z)))))
     pts)))
