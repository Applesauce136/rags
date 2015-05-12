#lang racket

(provide move scale rotate
         box sphere torus line)

(define move
  (lambda (x y z)
    (lambda (pixel)
      (map +
           pixel '(x y z)))))
(define scale
  (lambda (x y z)
    (lambda (pixel)
      (map *
           pixel '(x y z)))))
(define rotate
  (lambda (axis_s angle_d)
    (define axis (string->symbol axis_s))
    (define angle (degrees->radians angle_d))
     (lambda (pixel)
       (let ((x (first pixel))
             (y (second pixel))
             (z (third pixel)))
         (cond ((eq? 'x axis)
                (list x
                      (- (* y (cos angle))
                         (* z (sin angle)))
                      (+ (* y (sin angle))
                         (* z (cos angle)))))
               ((eq? 'y axis)
                (list (- (* x (cos angle))
                         (* z (sin angle)))
                      y
                      (+ (* x (sin angle))
                         (* z (cos angle)))))
               ((eq? 'z axis)
                (list (- (* x (cos angle))
                         (* y (sin angle)))
                      (+ (* x (sin angle))
                         (* y (cos angle)))
                      z)))))))

(define box
  (lambda (transforms x y z width height depth)
    '()))
(define sphere
  (lambda (transforms x y z radius)
    '()))
(define torus
  (lambda (transforms x y z t_radius c_radius)
    '()))
(define line
  (lambda (transforms x0 y0 z0 x1 y1 z1)
    '()))
