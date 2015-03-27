#lang racket

(require "../contracts.rkt")

(provide (contract-out
          (box
           (-> point/c real? real? real?
               (listof point/c)))
          (sphere
           (-> point/c real?
               (listof point/c)))
          (torus
           (-> point/c real? real?
               (listof point/c)))))

(define box
  (lambda (pt width height depth)
    '()))

(define sphere
  (lambda (pt radius)
    '()))

(define torus
  (lambda (pt c-radius t-radius)
    '()))
