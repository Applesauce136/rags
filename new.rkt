#lang racket

(require racket/gui/base)

;; (define my-bitmap (make-bitmap 500 500))

(define frame (new frame%
                   (label "DRAW THING")
                   (width 300)
                   (height 300)))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (define code (send event get-key-code))
      (when (char? code)
        (send keyboard-msg set-label
              (format "keycode: \"~a\""
                      (send event get-key-code)))))
    (define/override (on-event event)
      (send mouse-msg set-label
            (format "x: ~a y: ~a"
                    (send event get-x)
                    (send event get-y))))
    (super-new)))

(define panel-main (new horizontal-panel% (parent frame)))

(define my-canvas
  (new my-canvas% (parent panel-main)
       (min-width 500)
       (min-height 500)))

(define panel-controls (new vertical-panel% (parent panel-main)))
(define button (new button% (parent panel-controls)
                    (label "LEFF")))
(define mouse-msg (new message% (parent panel-controls)
                 (min-width 200)
                 (label "bah")))
(define keyboard-msg (new message% (parent panel-controls)
                 (min-width 200)
                 (label "bah")))

(send frame show #t)
