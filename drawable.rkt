#lang racket

(require "primitives.rkt")
(provide line%
         triangle%)

(define drawable<%>
  (interface ()
    set-index inc-index set-point
    draw name description))

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
