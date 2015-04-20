#lang racket

(require "draw-primitives.rkt")
(provide drawable%
         line%
         triangle%)

(define drawable%
  (class object%
    (super-new)

    (define pts (vector))
    (define pts-defaults (vector))
    
    (define/public draw
      (lambda (pt)
        '()))
    (define/public name
      (lambda ()
        ""))
    (define/public description
      (lambda ()
        ""))))

(define line%
  (class drawable%
    (super-new)
    (init pt)

    (define pts (vector pt #f))
    (define pts-defaults (vector (lambda ()
                                   (vector-ref pts 0))
                                 (lambda ()
                                   (vector-ref pts 0))))
    (define cur-index 0)
    
    (define/public in-bounds
      (lambda (index)
        (<= 0 index (- (vector-length pts-defaults) 1))))
    
    (define/public set-index
      (lambda (index)
        (when (or (= index -1)
                  (in-bounds index))
          (set! cur-index index))))

    (define/public inc-index
      (lambda ()
        (if (not (in-bounds (+ cur-index 1)))
            #f
            (set-index (+ cur-index 1)))))
    
    (define/public set-point
      (lambda (pt)
        (when (in-bounds cur-index)
          (vector-set! pts cur-index pt))))

    (define/override draw
      (lambda (new-pt)
        (define draw-this (vector-copy pts))
        (when (in-bounds cur-index)
          (vector-set! draw-this cur-index new-pt))
        (apply draw-line
               (vector->list (vector-map
                              (lambda (pt proc)
                                (if pt
                                    pt
                                    (proc)))
                              draw-this
                              pts-defaults)))))
    (define/override name
      (lambda ()
        "Line"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Line:\n"
         (list (format "~aFirst endpoint: ~a"
                       (if (= cur-index 0)
                           "*"
                           "")
                       (vector-ref pts 0))
               (format "~aSecond endpoint: ~a"
                       (if (= cur-index 1)
                           "*"
                           "")
                       (vector-ref pts 1)))
         "\n")))))

(define triangle%
  (class drawable%
    (super-new)
    (init pt)

    (define pts (vector pt #f #f))
    (define pts-defaults (vector (lambda ()
                                   (vector-ref pts 0))
                                 (lambda ()
                                   (vector-ref pts 0))
                                 (lambda ()
                                   (vector-ref pts 0))))
    (define cur-index 0)
    
    (define/public in-bounds
      (lambda (index)
        (<= 0 index (- (vector-length pts-defaults) 1))))
    
    (define/public set-index
      (lambda (index)
        (when (or (= index -1)
                  (in-bounds index))
          (set! cur-index index))))

    (define/public inc-index
      (lambda ()
        (if (not (in-bounds (+ cur-index 1)))
            #f
            (set-index (+ cur-index 1)))))
    
    (define/public set-point
      (lambda (pt)
        (when (in-bounds cur-index)
          (vector-set! pts cur-index pt))))
    
    (define/override draw
      (lambda (new-pt)
        (define draw-this (vector-copy pts))
        (when (in-bounds cur-index)
          (vector-set! draw-this cur-index new-pt))
        (draw-triangle
         (vector->list (vector-map
                        (lambda (pt proc)
                          (if pt
                              pt
                              (proc)))
                        draw-this
                        pts-defaults)))))
    (define/override name
      (lambda ()
        "Triangle"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Triangle:\n"
         (list (format "~aFirst vertex: ~a"
                       (if (= cur-index 0)
                           "*"
                           "")
                       (vector-ref pts 0))
               (format "~aSecond vertex: ~a"
                       (if (= cur-index 1)
                           "*"
                           "")
                       (vector-ref pts 1))
                              (format "~aThird vertex: ~a"
                       (if (= cur-index 2)
                           "*"
                           "")
                       (vector-ref pts 2)))
         "\n")))))
