#lang racket

(require racket/gui/base
         "draw-base.rkt")

(define width 800)
(define height 600)

(define drawables '())
(set! drawables (cons (new line%)
                      drawables))
(define cur-index 0)
(define current (list-ref drawables cur-index))
(send current set-index 1)
(send current set-point '(50 50 0))

(define my-bitmap (make-bitmap width height))
(define my-bitmap-dc (new bitmap-dc% (bitmap my-bitmap)))
(define update-bitmap
  (lambda ()
    (send my-bitmap-dc clear)
    (map
     (lambda (drawable)
       (map
        (lambda (pt)
          (send my-bitmap-dc set-pixel
                (first pt) (second pt) (make-color 0 0 0)))
        (send drawable pointify)))
     drawables)))


(define clicks '())

(define frame (new frame%
                   (label "DRAW THING")))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (define code (send event get-key-code))
      (send keypress set-label
            (format "keycode: \"~a\""
                    code)))
    (define/override (on-event event)
      (define loc `(,(send event get-x)
                    ,(send event get-y)
                    0)) 
      (define location-string
        (format "x: ~a y: ~a"
                (send event get-x)
                (send event get-y)))
      (send current set-point loc)
      (send location set-label
            location-string)
      (write-points)
      (when (send event button-down? 'left)
        (send current inc-index))
      (on-paint))
    (define/override (on-paint)
      (define my-canvas-dc (send my-canvas get-dc))
      (update-bitmap)
      (send my-canvas-dc draw-bitmap my-bitmap 0 0))
    (super-new)))

(define panel-main (new horizontal-panel% (parent frame)))

(define my-canvas
  (new my-canvas% (parent panel-main)
       (min-width width)
       (min-height height)))

(define panel-controls (new vertical-panel% (parent panel-main)))
(define button (new button% (parent panel-controls)
                    (label "LEFF")))
(define location (new message% (parent panel-controls)
                 (label "bah")
                 (auto-resize #t)))
(define keypress (new message% (parent panel-controls)
                 (label "bah")
                 (auto-resize #t)))
(define points (new message% (parent panel-controls)
                    (label "click")
                    (auto-resize #t)))
(define write-points
  (lambda ()
    (send points set-label
          (string-join
           #:before-first "Clicks:\n"
           (map
            (lambda (pt)
              (format "x: ~a y: ~a"
                      (first pt)
                      (second pt)))
            clicks)
           "\n"))))

(send frame show #t)
