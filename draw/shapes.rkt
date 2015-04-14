#lang racket

(require "../contracts.rkt")

(provide (contract-out
          (box
           (-> point/c real? real? real?
               (listof triangle/c)))
          (sphere
           (-> exact-positive-integer?
               point/c real?
               (listof triangle/c)))
          (torus
           (-> exact-positive-integer?
               point/c real? real?
               (listof triangle/c)))))

(define box
  (lambda (pt width height depth)
    (define x (first pt))
    (define y (second pt))
    (define z (third pt))
    `(
      ;; front face
      ((,x ,y ,z)
       (,(+ x width) ,(+ y height) ,z)
       (,(+ x width) ,y ,z))
      ((,x ,y ,z)
       (,x ,(+ y height) ,z)
       (,(+ x width) ,(+ y height) ,z))
      ;; back face
      ((,(+ x width) ,(+ y height) ,(+ z depth))
       (,x ,y ,(+ z depth))
       (,(+ x width) ,y ,(+ z depth)))
      ((,(+ x width) ,(+ y height) ,(+ z depth))
       (,x ,(+ y height) ,(+ z depth))
       (,x ,y ,(+ z depth)))
      ;; top face
      ((,x ,y ,z)
       (,(+ x width) ,y ,z)
       (,(+ x width) ,y ,(+ z depth)))
      ((,x ,y ,z)
       (,(+ x width) ,y ,(+ z depth))
       (,x ,y ,(+ z depth)))
      ;; bottom face
      ((,(+ x width) ,(+ y height) ,(+ z depth))
       (,x ,(+ y height) ,(+ z depth))
       (,(+ x width) ,(+ y height) ,z))
      ((,(+ x width) ,(+ y height) ,z)
       (,x ,(+ y height) ,z)
       (,x ,(+ y height) ,(+ z depth)))
      ;; left face
      ((,x ,y ,z)
       (,x ,y ,(+ z depth))
       (,x ,(+ y height) ,(+ z depth)))
      ((,x ,y ,z)
       (,x ,(+ y height) ,(+ z depth))
       (,x ,(+ y height) ,z))
      ;; right face
      ((,(+ x width) ,(+ y height) ,(+ z depth))
       (,(+ x width) ,y ,z)
       (,(+ x width) ,(+ y height) ,z))
      ((,(+ x width) ,(+ y height) ,(+ z depth))
       (,(+ x width) ,y ,(+ z depth))
       (,(+ x width) ,y ,z))
      )))

(define sphere
  (lambda (steps pt radius)
    '()))

(define torus
  (lambda (steps pt rad-t rad-c)
    '()))
