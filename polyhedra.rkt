#lang racket

(require "drawable.rkt"
         "primitives.rkt")

(provide (all-defined-out))

(define 3d-steps 5)

;; (define box%
;;   (class drawable%
;;     (init init-pt)
;;     (super-new (init-pt init-pt))

;;     (inherit-field current pts)
;;     (inherit draw set-index inc-index set-point)
;;     (set-mcdr! current (mcons #f '()))

;;     (define/override pointify
;;       (lambda (pts)
;;         (apply draw-line pts)))
;;     (define/override name
;;       (lambda ()
;;         "Line"))
;;     (define/override description
;;       (lambda ()
;;         (string-join
;;          #:before-first "Box:\n"
;;          (map (curry format "~a ~a: ~a")
;;               (map (lambda (pt)
;;                      (if (eq? pt (mcar current))
;;                          "*"
;;                          ""))
;;                    (mlist->list pts))
;;               (list "First" "Second")
;;               (mlist->list pts)) 
;;          "\n"))))
    
  ;; (lambda (pt width height depth)
  ;;   (define x (first pt))
  ;;   (define y (second pt))
  ;;   (define z (third pt))
  ;;   `(
  ;;     ;; front face
  ;;     ((,x ,y ,z)
  ;;      (,(+ x width) ,(+ y height) ,z)
  ;;      (,(+ x width) ,y ,z))
  ;;     ((,x ,y ,z)
  ;;      (,x ,(+ y height) ,z)
  ;;      (,(+ x width) ,(+ y height) ,z))
  ;;     ;; back face
  ;;     ((,(+ x width) ,(+ y height) ,(+ z depth))
  ;;      (,x ,y ,(+ z depth))
  ;;      (,(+ x width) ,y ,(+ z depth)))
  ;;     ((,(+ x width) ,(+ y height) ,(+ z depth))
  ;;      (,x ,(+ y height) ,(+ z depth))
  ;;      (,x ,y ,(+ z depth)))
  ;;     ;; top face
  ;;     ((,x ,y ,z)
  ;;      (,(+ x width) ,y ,z)
  ;;      (,(+ x width) ,y ,(+ z depth)))
  ;;     ((,x ,y ,z)
  ;;      (,(+ x width) ,y ,(+ z depth))
  ;;      (,x ,y ,(+ z depth)))
  ;;     ;; bottom face
  ;;     ((,(+ x width) ,(+ y height) ,(+ z depth))
  ;;      (,x ,(+ y height) ,(+ z depth))
  ;;      (,(+ x width) ,(+ y height) ,z))
  ;;     ((,(+ x width) ,(+ y height) ,z)
  ;;      (,x ,(+ y height) ,z)
  ;;      (,x ,(+ y height) ,(+ z depth)))
  ;;     ;; left face
  ;;     ((,x ,y ,z)
  ;;      (,x ,y ,(+ z depth))
  ;;      (,x ,(+ y height) ,(+ z depth)))
  ;;     ((,x ,y ,z)
  ;;      (,x ,(+ y height) ,(+ z depth))
  ;;      (,x ,(+ y height) ,z))
  ;;     ;; right face
  ;;     ((,(+ x width) ,(+ y height) ,(+ z depth))
  ;;      (,(+ x width) ,y ,z)
  ;;      (,(+ x width) ,(+ y height) ,z))
  ;;     ((,(+ x width) ,(+ y height) ,(+ z depth))
  ;;      (,(+ x width) ,y ,(+ z depth))
  ;;      (,(+ x width) ,y ,z))
  ;;     )))

(define torus%
  (class drawable%
    (init init-pt)
    (super-new (init-pt init-pt))

    (inherit-field current pts)
    (inherit draw set-index inc-index set-point)
    (set-mcdr! current (mcons #f (mcons #f '())))

    (define/override pointify
      (lambda (pts)
        (define rad-t (apply point-distance (take pts 2)))
        (define rad-c (apply point-distance (rest pts)))
        (define t (make-circle `(,rad-t 0 0) rad-c 3d-steps))
        (append-map (curry apply draw-triangle)
                    (append-map
                     (lambda (step)
                       (define t1
                         (map (curry rotate-point 'y (* 360 (/ step 3d-steps))) t))
                       (define t2
                         (map (curry rotate-point 'y (* 360 (/ (+ step 1) 3d-steps))) t)) 
                       (map list t1 (append (rest t1) (list (first t1))) t2))
                     (build-list 3d-steps identity)))))
    (define/override name
      (lambda ()
        "Torus"))
    (define/override description
      (lambda ()
        (string-join
         #:before-first "Torus:\n"
         (map (curry format "~a~a: ~a")
              (map (lambda (pt)
                     (if (eq? pt (mcar current))
                         "*"
                         ""))
                   (mlist->list pts))
              (list "Center" "Toroidial radius" "Inner radius")
              (mlist->list pts)) 
         "\n")))))

;; (define sphere
;;   (lambda (steps pt radius)
;;     (torus steps pt radius 0)))

