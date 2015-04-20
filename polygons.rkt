#lang racket

(require "primitives.rkt")
(require "drawable.rkt")
(provide (all-defined-out))

(define steps 10)

(define line%
  (class* object% (drawable<%>)
    (super-new)
    (init pt)

    (define pt0 pt)
    (define pt1 #f)
    (define pt1-proc
      (lambda ()
        (if pt1
            pt1
            pt0)))

    (define cur-index 0)

    (define/public set-index
      (lambda (index)
        (when (<= -1 index 1)
          (set! cur-index index))))

    (define/public inc-index
      (lambda ()
        (if (= cur-index 1)
            (begin
              (set-index -1)
              #f)
            (set-index (+ cur-index 1)))))

    (define/public set-point
      (lambda (pt)
        (cond ((= cur-index 0)
               (set! pt0 pt))
              ((= cur-index 1)
               (set! pt1 pt)))))

    (define internal-map
      (lambda (proc)
        (set! pt0 (map exact-floor (proc pt0)))
        (set! pt1 (map exact-floor (proc pt1)))))
    
    (define/public scale
      (lambda (x y z)
        (internal-map (curry scale-point x y z))))
    (define/public translate
      (lambda (x y z)
        (internal-map (curry translate-point x y z))))
    (define/public rotate
      (lambda (axis angle_d)
        (internal-map (curry rotate-point axis angle_d))))
    
    (define/public draw
      (lambda (new-pt)
        (draw-line
         (if (and new-pt (= cur-index 0)) new-pt pt0)
         (if (and new-pt (= cur-index 1)) new-pt (pt1-proc)))))
    
    (define/public name
      (lambda ()
        "Line"))
    (define/public description
      (lambda ()
        (string-join
         #:before-first "Line:\n"
         (list (format "~aFirst endpoint: ~a"
                       (if (= cur-index 0)
                           "*"
                           "")
                       pt0)
               (format "~aSecond endpoint: ~a"
                       (if (= cur-index 1)
                           "*"
                           "")
                       pt1))
         "\n")))))

(define triangle%
  (class object%
    (super-new)
    (init pt)

    (define pt0 pt)
    (define pt1 #f)
    (define pt1-proc
      (lambda ()
        (if pt1
            pt1
            pt0)))
    (define pt2 #f)
    (define pt2-proc
      (lambda ()
        (if pt2
            pt2
            pt0)))
    (define cur-index 0)
    
    (define/public set-index
      (lambda (index)
        (when (<= -1 index 2)
          (set! cur-index index))))
    
    (define/public inc-index
      (lambda ()
        (if (= cur-index 2)
            (begin
              (set! cur-index -1)
              #f)
            (set-index (+ cur-index 1)))))
    (define/public set-point
      (lambda (new-pt)
        (cond ((= cur-index 0)
               (set! pt0 new-pt))
              ((= cur-index 1)
               (set! pt1 new-pt))
              ((= cur-index 2)
               (set! pt2 new-pt)))))

    (define internal-map
      (lambda (proc)
        (set! pt0 (map exact-floor (proc pt0)))
        (set! pt1 (map exact-floor (proc pt1)))
        (set! pt2 (map exact-floor (proc pt2)))))
    
    (define/public scale
      (lambda (x y z)
        (internal-map (curry scale-point x y z))))
    (define/public translate
      (lambda (x y z)
        (internal-map (curry translate-point x y z))))
    (define/public rotate
      (lambda (axis angle_d)
        (internal-map (curry rotate-point axis angle_d))))
    
    (define/public draw
      (lambda (new-pt)
        (draw-triangle
         (if (and new-pt (= cur-index 0)) new-pt pt0)
         (if (and new-pt (= cur-index 1)) new-pt (pt1-proc))
         (if (and new-pt (= cur-index 2)) new-pt (pt2-proc)))))
    (define/public name
      (lambda ()
        "Triangle"))
    (define/public description
      (lambda ()
        (string-join
         #:before-first "Triangle:\n"
         (list (format "~aFirst vertex: ~a"
                       (if (= cur-index 0) "*" "")
                       pt0)
               (format "~aSecond vertex: ~a"
                       (if (= cur-index 1) "*" "")
                       pt1)
               (format "~aThird vertex: ~a"
                       (if (= cur-index 2) "*" "")
                       pt1))
         "\n")))))

(define circle%
  (class* object% (drawable<%>)
    (super-new)
    (init pt)
    
    (define pt0 pt)
    (define pt1 #f)
    (define pt1-proc
      (lambda ()
        (if pt1
            pt1
            pt0)))

    (define cur-index 0)

    (define/public set-index
      (lambda (index)
        (when (<= -1 index 1)
          (set! cur-index index))))

    (define/public inc-index
      (lambda ()
        (if (= cur-index 1)
            (begin
              (set-index -1)
              #f)
            (set-index (+ cur-index 1)))))

    (define/public set-point
      (lambda (pt)
        (cond ((= cur-index 0)
               (set! pt0 pt))
              ((= cur-index 1)
               (set! pt1 pt)))))

    (define internal-map
      (lambda (proc)
        (set! pt0 (map exact-floor (proc pt0)))
        (set! pt1 (map exact-floor (proc pt1)))))
    
    (define/public scale
      (lambda (x y z)
        (internal-map (curry scale-point x y z))))
    (define/public translate
      (lambda (x y z)
        (internal-map (curry translate-point x y z))))
    (define/public rotate
      (lambda (axis angle_d)
        (internal-map (curry rotate-point axis angle_d))))
    
    (define/public draw
      (lambda (new-pt)
        (define circle-proc
          (lambda (center rad-pt)
            (define x (list-ref center 0))
            (define y (list-ref center 1))
            (define z (list-ref center 2))
            (define r (distance center rad-pt))
            (map
             (lambda (step)
               (define angle (* 2 pi step))
               (list
                (+ x (* r (cos angle)))
                (+ y (* r (sin angle)))
                z))
             (build-list (+ steps 1) (lambda (n)
                                       (/ n steps))))))
        (define circle
          (circle-proc (if (and new-pt (= cur-index 0))
                           new-pt pt0)
                       (if (and new-pt (= cur-index 1))
                           new-pt (pt1-proc))))
        (append-map draw-line
                    circle (append (rest circle)
                                   (list (last circle))))))
    
    (define/public name
      (lambda ()
        "Circle"))
    (define/public description
      (lambda ()
        (string-join
         #:before-first "Line:\n"
         (list (format "~aCenter: ~a"
                       (if (= cur-index 0)
                           "*"
                           "")
                       pt0)
               (format "~aRadius: ~a"
                       (if (= cur-index 1)
                           "*"
                           "")
                       (distance pt0 (pt1-proc))))
         "\n")))))
