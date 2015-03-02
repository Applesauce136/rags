#lang racket

(require "draw.rkt"
         "display.rkt"
         "transform.rkt")

(define write-points
  (lambda (rows cols bg filename pts)
    (display (image->string (pixels->image rows cols bg
                                         pts))
             (open-output-file filename #:exists 'replace))))

(define write-default
  (curry write-points
         600 300 '(0 0 0) "pic.ppm"))
