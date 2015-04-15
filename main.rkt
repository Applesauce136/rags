#lang racket

(require racket/gui/base)

(define clicks '())

(define my-bitmap (make-bitmap 500 500))
(define my-bitmap-dc (new bitmap-dc% (bitmap my-bitmap)))
(define update-bitmap
  (lambda ()
    (send my-bitmap-dc clear)
    (map (lambda (pt)
           (send my-bitmap-dc set-pixel (first pt) (second pt) (make-color 0 0 0)))
         clicks)))

(define frame (new frame%
                   (label "DRAW THING")
                   (width 300)
                   (height 300)))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (define code (send event get-key-code))
      (when (char? code)
        (send keypress set-label
              (format "keycode: \"~a\""
                      (send event get-key-code)))))
    (define/override (on-event event)
      (define loc `(,(send event get-x)
                         ,(send event get-y)
                         0))
      (define location-string
        (format "x: ~a y: ~a"
                (send event get-x)
                (send event get-y)))
      (send location set-label
            location-string)
      (when (send event button-down? 'left)
        (set! clicks (cons loc clicks))
        (when (< 5 (length clicks))
          (set! clicks (take clicks 5)))
        (write-points))
      (on-paint))
    (define/override (on-paint)
      (define my-canvas-dc (send my-canvas get-dc))
      (update-bitmap)
      (send my-canvas-dc draw-bitmap my-bitmap 0 0))
    (super-new)))

(define panel-main (new horizontal-panel% (parent frame)))

(define my-canvas
  (new my-canvas% (parent panel-main)
       (min-width 500)
       (min-height 500)))

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
