#lang racket

(require racket/draw)

(provide
 matrix/c
 image/c
 pixel/c
 point/c
)

;; program flow is generally like:
;; generators -> points -> pixels -> image

;;; matrix: (vector of numbers)
(define matrix/c
  (flat-named-contract 'matrix
                       (vectorof
                        (vectorof number?
                                  #:flat? #t) #:flat? #t)))

;;; color: (r g b)
(define color/c
  (flat-named-contract 'color
                       (list/c (integer-in 0 255)
                               (integer-in 0 255)
                               (integer-in 0 255))))

;;; pixel: (row col color)
(define pixel/c
  (flat-named-contract 'pixel
                       (list/c exact-integer?
                               exact-integer?
                               (is-a?/c color%))))

;;; image: (rows cols max (vector of pixels))
(define image/c
  (flat-named-contract 'image
                       (list/c exact-nonnegative-integer?
                               exact-nonnegative-integer?
                               exact-nonnegative-integer?
                               (vectorof color/c #:flat? #t))))

;;; point: (x y z)
(define point/c
  (flat-named-contract 'point
                       (list/c real? real? real?)))
