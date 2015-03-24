#lang racket

(require "contracts.rkt"
         "matrix.rkt")

(provide (contract-out
          (circle
           (-> exact-positive-integer?
               point/c real?
               procedure?))
          (hermite-curve
           (-> exact-positive-integer?
               point/c point/c rational? rational?
               procedure?))
          (bezier-curve
           (-> exact-positive-integer?
               point/c point/c point/c point/c
               procedure?))))

(define circle
  (lambda (steps pt r)
    (define x (list-ref pt 0))
    (define y (list-ref pt 1))
    (define z (list-ref pt 2))
    (lambda (step)
      (define angle (* 2 pi (/ step steps)))
      (list
       (+ x (* r (cos angle)))
       (+ y (* r (sin angle)))
       z)
      )))

(define hermite-curve
  (lambda (steps p0 p1 r0 r1)
    (define x-curve
      (curve (make-matrix
              '(( 2 -2  1  1)
                (-3  3 -2 -1)
                ( 0  0  1  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(list-ref p0 0))
                (,(list-ref p1 0))
                (,(denominator r0))
                (,(denominator r1))))))
    (define y-curve
      (curve (make-matrix
              '(( 2 -2  1  1)
                (-3  3 -2 -1)
                ( 0  0  1  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(list-ref p0 1))
                (,(list-ref p1 1))
                (,(numerator r0))
                (,(numerator r1))))))
    (define z (list-ref p0 2))
    (lambda (step)
      (list (x-curve (/ step steps))
            (y-curve (/ step steps))
            z))))

(define bezier-curve
  (lambda (steps p0 p1 p2 p3)
    (define x-curve
      (curve (make-matrix
              '((-1  3 -3  1)
                ( 3  6  3  0)
                (-3  3  0  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(list-ref p0 0))
                (,(list-ref p1 0))
                (,(list-ref p2 0))
                (,(list-ref p3 0))))))
    (define y-curve
      (curve (make-matrix
              '((-1  3 -3  1)
                ( 3  6  3  0)
                (-3  3  0  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(list-ref p0 1))
                (,(list-ref p1 1))
                (,(list-ref p2 1))
                (,(list-ref p3 1))))))
    (define z (list-ref p0 2))
    (lambda (step)
      (list (x-curve (/ step steps))
            (y-curve (/ step steps))
            z))))

(define curve
  (lambda (c-mtx dat-mtx)
    (apply make-poly
           (vector->list
            (matrix-col
             (matrix-multiply c-mtx dat-mtx)
             0)))))

(define make-poly
  (lambda coeffs
    (lambda (x)
      (foldl + 0
             (map
              (lambda (coeff pow)
                (* coeff (expt x pow)))
              (reverse coeffs)
              (build-list (length coeffs) identity))))))
