#lang racket

(provide origins
         push pop
         move scale rotate
         box sphere torus line
         save)

(define origins '())

(define push
  (lambda ()
    '()))
(define pop
  (lambda ()
    '()))

(define move
  (lambda (x y z)
    '()))
(define sncale
  (lambda (x y z)
    '()))
(define rotate
  (lambda (axis angle_d)
    '()))

(define box
  (lambda (x y z width height depth)
    '()))
(define sphere
  (lambda (x y z radius)
    '()))
(define torus
  (lambda (x y z t_radius c_radius)
    '()))
(define line
  (lambda (x0 y0 z0 x1 y1 z1)
    '()))

(define save
  (lambda (filename)
    '()))
