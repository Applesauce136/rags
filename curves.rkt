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
               point/c rational? point/c rational?
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
    (curve steps
           (make-matrix
            '(( 2 -2  1  1)
              (-3  3 -2 -1)
              ( 0  0  1  0)
              ( 1  0  0  0)))
           (make-matrix
            `((,p0)
              (,p1)
              (,r0)
              (,r1))))))

(define bezier-curve
  (lambda (steps p0 p1 p2 p3)
    (curve steps
           (make-matrix
            '(( 2 -2  1  1)
              (-3  3 -2 -1)
              ( 0  0  1  0)
              ( 1  0  0  0)))
           (make-matrix
            `((,p0)
              (,p1)
              (,p2)
              (,p3))))))

(define curve
  (lambda (steps c-mtx dat-mtx)
    (make-poly (list->vector
                (matrix-col
                 (matrix-multiply c-mtx dat-mtx)
                 0)))))

(define make-poly
  (lambda coeffs
    (lambda (x)
      (foldl +
             (map
              (lambda (coeff pow)
                (* coeff (expt x pow)))
              (reverse coeffs)
              (build-list (length coeffs) identity))))))
