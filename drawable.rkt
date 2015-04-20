#lang racket

(provide drawable<%>)

(define drawable<%>
  (interface ()
    set-index inc-index set-point
    translate scale rotate
    draw name description))
