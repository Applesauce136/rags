#lang racket

(require "contracts.rkt"
         "matrix.rkt")

(provide (contract-out
          (circle
           (-> point/c real?
               procedure?))
          (hermite-curve
           (-> point/c point/c rational? rational?
               procedure?))
          (bezier-curve
           (-> point/c point/c point/c point/c
               procedure?))))

(define circle
  (lambda (pt r)
    (define x (list-ref pt 0))
    (define y (list-ref pt 1))
    (define z (list-ref pt 2))
    (lambda (step)
      (define angle (* 2 pi step))
      (list
       (+ x (* r (cos angle)))
       (+ y (* r (sin angle)))
       z)
      )))

(define hermite-curve
  (lambda (p0 p1 r0 r1)
    (define x-curve
      (curve (make-matrix
              '(( 2 -2  1  1)
                (-3  3 -2 -1)
                ( 0  0  1  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(first p0))
                (,(first p1))
                (,r0)
                (,r1)))))
    (define y-curve
      (curve (make-matrix
              '(( 2 -2  1  1)
                (-3  3 -2 -1)
                ( 0  0  1  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(second p0))
                (,(second p1))
                (,r0)
                (,r1)))))
    (define z (third p0))
    (lambda (step)
      (list (x-curve step)
            (y-curve step)
            z))))

(define bezier-curve
  (lambda (p0 p1 p2 p3)
    (define x-curve
      (curve (make-matrix
              '((-1  3 -3  1)
                ( 3  6  3  0)
                (-3  3  0  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(first p0))
                (,(first p1))
                (,(first p2))
                (,(first p3))))))
    (define y-curve
      (curve (make-matrix
              '((-1  3 -3  1)
                ( 3  6  3  0)
                (-3  3  0  0)
                ( 1  0  0  0)))
             (make-matrix
              `((,(second p0))
                (,(second p1))
                (,(second p2))
                (,(second p3))))))
    (define z (third p0))
    (lambda (step)
      (list (x-curve step)
            (y-curve step)
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
