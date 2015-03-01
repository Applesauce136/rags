#lang racket

(require "draw.rkt"
         "display.rkt"
         "transform.rkt")

(define write-pts
  (lambda (rows cols bg filename pts)
    (display (image->string (pixs->image rows cols bg
                                         pts))
             (open-output-file filename #:exists 'replace))))

(define write-default
  (curry write-pts
         600 400 '(0 0 0) "pic.ppm"))
