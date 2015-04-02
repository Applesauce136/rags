#lang racket

(require racket/gui/base
         "main.rkt")

(define my-bitmap (make-monochrome-bitmap 100 100))

(define frame (new frame%
                   (label "Example")
                   (width 300)
                   (height 300)))

(define panel-main (new horizontal-panel% (parent frame)))

(new canvas% (parent panel-main)
     (paint-callback
      (lambda (canvas dc)
        (send dc draw-bitmap my-bitmap 0 0))))

(define panel-controls (new vertical-panel% (parent panel-main)))

(new button% (parent panel-controls)
     (label "LEFF"))

(send frame show #t)
