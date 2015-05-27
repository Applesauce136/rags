#lang racket

(provide (all-defined-out))

(define move-point
  (lambda (x y z)
    (lambda (pixel)
      (map +
           pixel (list x y z)))))
(define scale-point
  (lambda (x y z)
    (lambda (pixel)
      (map *
           pixel (list x y z)))))
(define rotate-point
  (lambda (axis angle_d)
    (define angle (degrees->radians angle_d))
    (lambda (pixel)
      (call-with-values (lambda () (apply values pixel))
        (lambda (x y z)
          (map exact-floor
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
                            z))
                     (else
                      (printf "invalid axis: ~s~n" axis)
                      pixel))))))))
