#lang racket

(require racket/generator)

(provide (all-defined-out))

(define mash-generators
  (lambda gens
    ;; mashes together a list of generators into one big one
    (generator
     ()
     (let one-line ((lines gens))
       (if (null? lines)
           #f
           (begin
             (let ((val ((car lines))))
               (cond (val (begin (yield val)
                                 (one-line lines)))
                     (else (one-line (cdr lines)))))))))))

(define draw-triangle 
  (lambda (p0 p1 p2)
    (call-with-values (lambda ()
                        (apply values
                               (sort (list p0 p1 p2)
                                     <
                                     #:key first)))
      (lambda (n0 n1 n2)
        (printf "    n0: ~a~nn1: ~a~nn2: ~a~n" n0 n1 n2)
        (call-with-values (lambda ()
                            (values (draw-line n0 n1)
                                    (draw-line n1 n2)
                                    (draw-line n0 n2)))
          (lambda (first-half second-half base)
            (printf "    up to (first-half second-half base)")
            (define ends (mash-generators first-half second-half))
            (define only-if-x-moves
              (lambda (gen)
                (generator
                 ()
                 (let one-pixel ((pixel (gen))
                                 (x -1))
                   (cond ((not pixel)
                          #f)
                         ((< x (car pixel))
                          (begin (yield pixel)
                                 (one-pixel (gen) (car pixel))))
                         (else
                          (one-pixel (gen) x)))))))
            (define new-base (only-if-x-moves base))
            (define new-ends (only-if-x-moves ends))
            (generator
             ()
             (let line ((e0 (new-base))
                        (e1 (new-ends)))
               (if (and e0 e1)
                   (begin
                     (let ((one-line (draw-line e0 e1)))
                       (let pixel ((val (one-line)))
                         (if val
                             (begin (yield val)
                                    (pixel (one-line)))
                             #f)))
                     (line (new-base) (new-ends)))
                   #f))))) 
        ))
    ;; (mash-generators (draw-line n0 n1)
    ;;                  (draw-line n1 n2)
    ;;                  (draw-line n2 n0))
    ))

(define draw-line 
  (lambda (pt0 pt1)
    (generator
     ()
     (let*-values
         (((x0 y0 z0) (apply values pt0))
          ((x1 y1 z1) (apply values pt1))
          ((dx) (abs (- x1 x0)))
          ((dy) (abs (- y1 y0)))
          ((pri) (cond ((<= dy dx) 'x)
                       ((<= dx dy) 'y)))
          ((sec) (cond ((eq? pri 'x) 'y)
                       ((eq? pri 'y) 'x)))
          ((pri-i) (cond ((eq? pri 'x) x0)
                         ((eq? pri 'y) y0)))
          ((pri-f) (cond ((eq? pri 'x) x1)
                         ((eq? pri 'y) y1)))
          ((sec-i) (cond ((eq? sec 'x) x0)
                         ((eq? sec 'y) y0)))
          ((sec-f) (cond ((eq? sec 'x) x1)
                         ((eq? sec 'y) y1)))
          ((pri-dir) (if (<= pri-i pri-f) 1 -1))
          ((sec-dir) (if (<= sec-i sec-f) 1 -1))
          ((a) (* pri-dir 2 (- sec-f sec-i)))
          ((b) (* sec-dir 2 (- pri-i pri-f)))
          ((check-mp?) (if (< 0 (* pri-dir sec-dir))
                           (lambda (mp) (> mp 0))
                           (lambda (mp) (< mp 0)))))
       (let pixel
           ((mp (+ a (/ b 2)))
            (pri-c pri-i)
            (sec-c sec-i))
         (if (= pri-c (+ pri-f 1))
             #f
             (begin
               (yield (cond ((eq? pri 'x) (list sec-c pri-c z0))
                            ((eq? pri 'y) (list pri-c sec-c z0))))
               (pixel
                (+ mp a (if (check-mp? mp) b 0))
                (+ pri-c pri-dir)
                (+ sec-c (if (check-mp? mp) sec-dir 0))))))))))

(define frontface?
  (lambda (triangle)
    (call-with-values (lambda ()
                        (apply values triangle))
      (lambda (p0 p1 p2)
        (call-with-values (lambda ()
                            (values (map - p1 p0)
                                    (map - p2 p1)))
          (lambda (a b)
            (let ((normal (cross-product a b)))
              (<= (dot-product normal '(0 0 -1)) 0))))))))

(define cross-product
  (lambda (a b)
    (call-with-values (lambda ()
                        (apply values (append a b)))
      (lambda (ax ay az bx by bz)
        (list (- (* ay bz) (* az by))
              (- (* az bx) (* ax bz))
              (- (* ax by) (* ay bx)))))))

(define dot-product
  (lambda (a b)
    (foldl + 0 (map * a b))))
