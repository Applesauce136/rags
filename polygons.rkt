#lang racket

(require "drawable.rkt"
         "primitives.rkt")
(provide (all-defined-out))

(define steps 20)

(define line%
  (class drawable%
    (init init-pt)
    (super-new (init-pt init-pt))

    (inherit-field current pts)
    (inherit draw set-index inc-index set-point)
    (set-mcdr! current (mcons #f '()))

    (define/override pointify
      (lambda (pts)
        (apply draw-line pts)))
    (define/override name
      (lambda ()
        "Line"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Line:\n"
         (map (curry format "~a ~a endpoint: ~a")
              (map (lambda (pt)
                     (if (eq? pt (mcar current))
                         "*"
                         ""))
                   (mlist->list pts))
              (list "First" "Second")
              (mlist->list pts)) 
         "\n")))))

(define triangle%
  (class drawable%
    (init init-pt)
    (super-new (init-pt init-pt))

    (inherit-field current pts)
    (inherit draw set-index inc-index set-point)
    (set-mcdr! current (mcons #f (mcons #f '())))
    
    (define/override pointify
      (lambda (pts)
        (apply draw-triangle pts)))
    (define/override name
      (lambda ()
        "Triangle"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Triangle:\n"
         (map (curry format "~a ~a vertex: ~a")
              (map (lambda (pt)
                     (if (eq? pt (mcar current))
                         "*"
                         ""))
                   (mlist->list pts))
              (list "First" "Second" "Third")
              (mlist->list pts)) 
         "\n")))))

(define circle%
  (class drawable%
    (init init-pt)
    (super-new (init-pt init-pt))

    (inherit-field current pts)
    (inherit draw set-index inc-index set-point)
    (set-mcdr! current (mcons #f '()))

    (define/override pointify
      (lambda (pts)
        (define circle (make-circle (first pts)
                                    (apply point-distance pts)
                                    steps))
        (append-map draw-line circle (append (rest circle) (list (last circle))))))
    
    (define/override name
      (lambda ()
        "Circle"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Circle:\n"
         (map (curry format "~a~a: ~a")
              (map (lambda (pt)
                     (if (eq? pt (mcar current))
                         "*"
                         ""))
                   (mlist->list pts))
              (list "Center" "Radius")
              (mlist->list pts)) 
         "\n")))))
