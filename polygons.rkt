#lang racket

(require "drawable.rkt"
         "primitives.rkt")
(provide (all-defined-out))

(define steps 10)

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
        (define center (first pts))
        (define rad-pt (second pts))
        (define x (list-ref center 0))
        (define y (list-ref center 1))
        (define z (list-ref center 2))
        (define r (point-distance center rad-pt))
        (map
         (lambda (step)
           (define angle (* 2 pi step))
           (list
            (+ x (* r (cos angle)))
            (+ y (* r (sin angle)))
            z))
         (list 0 1 2 3 4 5 6)
         ;; (build-list (+ steps 1) (lambda (n)
         ;;                           (/ n steps)))
         )))
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
