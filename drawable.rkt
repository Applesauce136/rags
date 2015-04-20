#lang racket

(require "primitives.rkt")
(provide drawable%)

(define drawable%
  (class object%
    (super-new)
    (init init-pt)

    (field (degrees-x 0)
           (degrees-y 0)
           (pts (mcons init-pt '()))
           (current pts)
           (anchor init-pt))

    (define/public rotate-x
      (lambda (degrees)
        (set! degrees-x (+ degrees-x degrees))))
    (define/public rotate-y
      (lambda (degrees)
        (set! degrees-y (+ degrees-y degrees))))
    (define/public translate
      (lambda (x y z)
        (set! anchor (map + anchor (list x y z)))))
    
    (define/public draw
      (lambda (new-pt)
        (map (compose (curry map exact-floor)
                      (apply (curry translate-point) anchor)
                      (curry rotate-point 'x degrees-x)
                      (curry rotate-point 'y degrees-y))
             (pointify (map
                        (lambda (pt)
                          (cond ((and new-pt
                                      (eq? pt (mcar current)))
                                 (point-diff anchor new-pt))
                                ((not pt)
                                 (mcar pts))
                                (else
                                 pt)))
                        (mlist->list pts))))))
    
    (define/public set-index
      (lambda (index)
        (when (<= 0 index (- (length (mlist->list pts)) 1))
          (define mlist-ref
            (lambda (mlist index)
              (if (= index 0)
                  mlist
                  (mlist-ref (mcdr mlist) (- index 1)))))
          (set! current (mlist-ref pts index)))))

    (define/public inc-index
      (lambda ()
        (cond ((eq? current '())
               (set! current pts))
              ((eq? (mcdr current) '()) ;; or, current is the last one
               #f)
              (else
               (set! current (mcdr current))))))

    (define/public set-point
      (lambda (pt)
        (when (not (eq? current '()))
          (set-mcar! current (point-diff anchor pt)))))

    (abstract pointify)
    (abstract name)
    (abstract description)))
