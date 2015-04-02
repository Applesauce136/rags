#lang racket

(require racket/gui/base
         ;; "main.rkt"
         )

(define my-bitmap (make-bitmap 500 500))
(define my-bitmap-dc (new bitmap-dc% (bitmap my-bitmap)))

(define frame (new frame%
                   (label "Example")
                   (width 300)
                   (height 300)))

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (send mouse-msg set-label
            (format "x: ~a y: ~a"
                    (send event get-x)
                    (send event get-y))))
    (define/override (on-char event)
      (define code (send event get-key-code))
      (when (char? code)
        (send keyboard-msg set-label
              (format "keycode: ~a"
                      (send event get-key-code)))))
    (super-new)))

(define panel-main (new horizontal-panel% (parent frame)))

(define my-canvas
  (new my-canvas% (parpent panel-main)
       (min-width 500)
       (min-height 500)
       (paint-callback
        (lambda (canvas dc)
          (send dc draw-bitmap my-bitmap 0 0)))))

(define panel-controls (new vertical-panel%
                            (parent panel-main)))
(new button% (parent panel-controls)
     (label "LEFF"))
(define mouse-msg (new message%
                 (parent panel-controls)
                 (min-width 200)
                 (label "bah")))
(define keyboard-msg (new message%
                 (parent panel-controls)
                 (min-width 200)
                 (label "bah")))

(map (lambda (n)
       (send my-bitmap-dc set-pixel n 50 (make-color 255 0 0)))
     (build-list 100 identity))

(send frame show #t)
