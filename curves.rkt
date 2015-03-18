#lang racket

(require "contracts.rkt")

(provide (contract-out
          (circle
           (-> point/c real?
               procedure?))
          (hermite-curve
           (-> point/c rational? point/c rational?
               procedure?))
          (bezier-curve
           (-> point/c point/c point/c point/c
               procedure?))))

(define circle
  (lambda (pt radius)
    '()))

(define hermite-curve
  (lambda (pt1 d1 pt2 d2)
    '()))

(define bezier-curve
  (lambda (pt1 pt pt3 pt4)
    '()))
