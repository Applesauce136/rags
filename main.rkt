#lang racket

(require racket/gui/base
         "draw-base.rkt")

(define width 800)
(define height 600)

(define drawables '())
(define current #f)
(define cur-index -1)
(define move-current
  (lambda (index)
    (when current
      (send current set-index -1))
    (when (< index (length drawables))
      (set! current (if (<= index -1)
                        #f
                        (list-ref drawables index)))
      (set! cur-index index))))

(define adding? #f)
(define add-drawable
  (lambda (drawable)
    (set! drawables (cons drawable drawables))
    (set! adding? #f)
    (set! current (first drawables))))
(define new-drawable line%)

(define my-bitmap (make-bitmap width height))
(define my-bitmap-dc (new bitmap-dc% (bitmap my-bitmap)))
(define update-bitmap
  (lambda ((pt #f))
    (send my-bitmap-dc clear)
    (map
     (lambda (drawable)
       (define color (make-color
                      (if (eq? current drawable)
                          255 0) 0 0))
       (map
        (lambda (pt)
          (define x (first pt))
          (define y (second pt))
          (when (and (<= 0 x width)
                     (<= 0 y height))
              (send my-bitmap-dc set-pixel
                    x y color)))
        (send drawable draw
              (if (eq? current drawable)
                  pt #f))))
     drawables)))
(define update-adding
  (lambda ()
    (send adding set-label
          (if adding?
              (format "ADDING A:~n~a" (send (new new-drawable (pt '(0 0 0))) name))
              ""))))
(define update-names
  (lambda ()
    (send names set-label
          (string-join
           #:before-first "Shapes:\n"
           (map
            (lambda (drawable)
              (string-append
               (if (eq? current drawable)
                   "*" "")
               (send drawable name)))
            drawables)
           "\n"))))
(define update-description
  (lambda ()
    (send description set-label
          (if current
              (send current description)
              ""))
    (send adding set-label
          (if adding?
              (string-append "ADDING A:\n"
                             (send (new new-drawable (pt '(0 0 0))) name))
              ""))))

(define my-canvas%
  (class canvas%

    (super-new)
    
    (define loc '(0 0 0))
    (define can-click? #t)
    
    (define/override (on-char char-event)
      (define code (send char-event get-key-code))
      (cond (adding?
             (cond ((eq? code 'escape)
                    (set! adding #f))
                   ((eq? code #\l)
                    (set! new-drawable line%))
                   ((eq? code #\t)
                    (set! new-drawable triangle%))))
            
            ((and (char? code)
                  (char-numeric? code)
                  current)
             (send current set-index
                   (- (char->integer code)
                      (char->integer #\1))))
            
            ((eq? code #\a)
             (set! adding? #t)
             (move-current -1))
            
            ((eq? code #\tab)
             (move-current (+ cur-index
                              (if (send char-event get-shift-down)
                                  -1 1)))))
      (send this refresh))
    (define/override (on-event mouse-event) 
      (set! loc
        (list (send mouse-event get-x)
              (send mouse-event get-y)
              0))
      (send location set-label
            (format "~a" loc))
      (if (send mouse-event get-left-down) 
          (when can-click?
            (when adding?
              (add-drawable (new new-drawable (pt loc))))
            (when current
              (send current set-point loc)
              (unless (send current inc-index)
                (move-current -1)))
            (set! can-click? #f))
          (set! can-click? #t))
      (send this refresh))
    (define/override (on-paint)
      (update-bitmap loc)
      (update-names)
      (update-adding)
      (update-description)
      (send (send my-canvas get-dc) draw-bitmap my-bitmap 0 0))))

(define frame (new frame%
                   (label "DRAW THING")))

(define main-panel (new vertical-panel% (parent frame)))
(define drawing-panel (new horizontal-panel% (parent main-panel)))

(define my-canvas
  (new my-canvas% (parent drawing-panel)
       (min-width width)
       (min-height height)))

(define control-panel (new vertical-panel% (parent drawing-panel)))
(define location (new message% (parent control-panel)
                      (label "hello!")
                      (auto-resize #t)))
(define adding (new message% (parent control-panel)
                    (label "")
                    (auto-resize #t)))
(define names (new message% (parent control-panel)
                   (label "")
                   (auto-resize #t)))
(define description (new message% (parent main-panel)
                         (label "")
                         (auto-resize #t)))

(send frame show #t)
