#lang racket

(require "../contracts.rkt")

(provide (contract-out
          (box
           (-> point/c real? real? real?
               (listof point/c)))
          (sphere
           (-> exact-positive-integer?
               point/c real?
               (listof point/c)))
          (torus
           (-> exact-positive-integer?
               point/c real? real?
               (listof point/c)))))

(define box
  (lambda (pt width height depth)
    '((0 0 0))))

(define sphere
  (lambda (steps pt radius)
    '((0 0 0))))

(define torus
  (lambda (steps pt rad-t rad-c)
    '((0 0 0))))
