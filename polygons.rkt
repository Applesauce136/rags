#lang racket

(require "drawable.rkt"
         "primitives.rkt")
(provide (all-defined-out))

(define 2d-steps 20)

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
                                    2d-steps))
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

(define hermite%
    (class drawable%
    (init init-pt)
    (super-new (init-pt init-pt))

    (inherit-field current pts)
    (inherit draw set-index inc-index set-point)
    (set-mcdr! current (mcons #f (mcons #f (mcons #f '()))))

    (define/override pointify
      (lambda (pts)
        (define p0 (first pts))
        (define p1 (second pts))
        (define r0 (point-slope p0 (third pts)))
        (define r1 (point-slope p1 (fourth pts)))
        (define x-curve
          (curve (make-matrix
                  '(( 2 -2  1  1)
                    (-3  3 -2 -1)
                    ( 0  0  1  0)
                    ( 1  0  0  0)))
                 (make-matrix
                  `((,(first p0))
                    (,(first p1))
                    (,r0)
                    (,r1)))))
        (define y-curve
          (curve (make-matrix
                  '(( 2 -2  1  1)
                    (-3  3 -2 -1)
                    ( 0  0  1  0)
                    ( 1  0  0  0)))
                 (make-matrix
                  `((,(second p0))
                    (,(second p1))
                    (,r0)
                    (,r1)))))
        (define z (third p0))
        (define gen-pts (map (lambda (step)
                           (list (x-curve step)
                                 (y-curve step)
                                 z))
                         (build-list 2d-steps
                                     (lambda (step)
                                       (/ step 2d-steps)))))
        (append-map draw-line
                    (take gen-pts (- (length gen-pts) 1))
                    (rest gen-pts))))
    
    (define/override name
      (lambda ()
        "Hermite"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Hermite Curve:\n"
         (map (curry format "~a~a: ~a")
              (map (lambda (pt)
                     (if (eq? pt (mcar current))
                         "*"
                         ""))
                   (mlist->list pts))
              (list "First Endpoint" "Second Endpoint"
                    "First Slope" "Second Slope")
              (list (mcar pts) (mcar (mcdr pts))
                    (if (mcar (mcdr (mcdr pts)))
                        (point-slope (mcar pts)
                                     (mcar (mcdr (mcdr pts))))
                        #f)
                    (if (mcar (mcdr (mcdr (mcdr pts))))
                        (point-slope (mcar (mcdr pts))
                                     (mcar (mcdr (mcdr (mcdr pts)))))
                        #f)))
         "\n")))))

(define bezier%
    (class drawable%
    (init init-pt)
    (super-new (init-pt init-pt))

    (inherit-field current pts)
    (inherit draw set-index inc-index set-point)
    (set-mcdr! current (mcons #f (mcons #f (mcons #f '()))))

    (define/override pointify
      (lambda (pts)
        (define p0 (first pts))
        (define p1 (second pts))
        (define p2 (third pts))
        (define p3 (fourth pts))        
        (define x-curve
          (curve (make-matrix
                  '((-1  3 -3  1)
                    ( 3 -6  3  0)
                    (-3  3  0  0)
                    ( 1  0  0  0)))
                 (make-matrix
                  `((,(first p0))
                    (,(first p1))
                    (,(first p2))
                    (,(first p3))))))
        (define y-curve
          (curve (make-matrix
                  '((-1  3 -3  1)
                    ( 3 -6  3  0)
                    (-3  3  0  0)
                    ( 1  0  0  0)))
                 (make-matrix
                  `((,(second p0))
                    (,(second p1))
                    (,(second p2))
                    (,(second p3))))))
        (define z (third p0))
        (define gen-pts (map (lambda (step)
                           (list (x-curve step)
                                 (y-curve step)
                                 z))
                         (build-list 2d-steps
                                     (lambda (step)
                                       (/ step 2d-steps)))))
        (append-map draw-line
                    (take gen-pts (- (length gen-pts) 1))
                    (rest gen-pts))))
    
    (define/override name
      (lambda ()
        "Bezier"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Bezier Curve:\n"
         (map (curry format "~a~a: ~a")
              (map (lambda (pt)
                     (if (eq? pt (mcar current))
                         "*"
                         ""))
                   (mlist->list pts))
              (list "First Endpoint" "First Influencer"
                    "Second Influencer" "Second Endpoint")
              (list (mcar pts)
                    (mcar (mcdr pts))
                    (mcar (mcdr (mcdr pts)))
                    (mcar (mcdr (mcdr (mcdr pts))))))
         "\n")))))
