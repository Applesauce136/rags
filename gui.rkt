#lang racket

(require racket/gui/base
         "main.rkt")

(define my-bitmap (make-bitmap 100 100))
(define my-bitmap-dc (new bitmap-dc% (bitmap my-bitmap)))

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

(map (lambda (n)
       (send my-bitmap-dc set-pixel n 50 (make-object color% 255 0 0)))
     (build-list 100 identity))

(send frame show #t)
