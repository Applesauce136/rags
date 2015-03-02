#lang racket

(provide
 image/c
 color/c
 pixel/c
 point/c
)

;;; color: (r g b)
(define color/c
  (flat-named-contract 'color
                       (list/c (integer-in 0 255)
                               (integer-in 0 255)
                               (integer-in 0 255))))

;;; pixel: (row col color)
(define pixel/c
  (flat-named-contract 'pixel
                       (list/c exact-nonnegative-integer?
                               exact-nonnegative-integer?
                               color/c)))

;;; image: (rows cols max (vector of pixels))
(define image/c
  (flat-named-contract 'image
                       (list/c exact-nonnegative-integer?
                               exact-nonnegative-integer?
                               exact-nonnegative-integer?
                               (vectorof pixel/c #:flat? #t))))

;;; point: (x y z)
(define point/c
  (flat-named-contract 'point
                       (list/c real? real? real?)))
