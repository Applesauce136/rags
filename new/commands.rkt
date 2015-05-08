#lang racket

(provide matrix-identity matrix-clone matrix-multiply
         move scale rotate
         box sphere torus line)

(define matrix-identity
  (lambda ()
    '()))

(define matrix-clone
  (lambda (mtx)
    '()))

(define matrix-multiply
  (lambda (mtx1 mtx2)
    '()))

(define move
  (lambda (x y z)
    '()))
(define scale
  (lambda (x y z)
    '()))
(define rotate
  (lambda (axis angle_d)
    '()))

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
