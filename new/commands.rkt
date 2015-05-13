#lang racket

(provide move scale rotate
         box sphere torus line)

(define steps 10)

(define move
  (lambda (x y z)
    (lambda (pixel)
      (map +
           pixel (list x y z)))))
(define scale
  (lambda (x y z)
    (lambda (pixel)
      (map *
           pixel (list x y z)))))
(define rotate
  (lambda (axis angle_d)
    (define angle (degrees->radians angle_d))
    (lambda (pixel)
      (define x (first pixel))
      (define y (second pixel))
      (define z (third pixel))
      (cond ((eq? 'x axis)
             (list x
                   (- (* y (cos angle))
                      (* z (sin angle)))
                   (+ (* y (sin angle))
                      (* z (cos angle)))))
            ((eq? 'y axis)
             (list (- (* x (cos angle))
                      (* z (sin angle)))
                   y
                   (+ (* x (sin angle))
                      (* z (cos angle)))))
            ((eq? 'z axis)
             (list (- (* x (cos angle))
                      (* y (sin angle)))
                   (+ (* x (sin angle))
                      (* y (cos angle)))
                   z))))))

(define box
  (lambda (transforms x y z width height depth)
    (append-map (lambda (triangle)
                  (apply draw-triangle
                         (map transforms
                              triangle)))
                `(;; front face
                  ((,x ,y ,z)
                   (,(+ x width) ,(+ y height) ,z)
                   (,(+ x width) ,y ,z))
                  ((,x ,y ,z)
                   (,x ,(+ y height) ,z)
                   (,(+ x width) ,(+ y height) ,z))
                  ;; back face
                  ((,(+ x width) ,(+ y height) ,(+ z depth))
                   (,x ,y ,(+ z depth))
                   (,(+ x width) ,y ,(+ z depth)))
                  ((,(+ x width) ,(+ y height) ,(+ z depth))
                   (,x ,(+ y height) ,(+ z depth))
                   (,x ,y ,(+ z depth)))
                  ;; top face
                  ((,x ,y ,z)
                   (,(+ x width) ,y ,z)
                   (,(+ x width) ,y ,(+ z depth)))
                  ((,x ,y ,z)
                   (,(+ x width) ,y ,(+ z depth))
                   (,x ,y ,(+ z depth)))
                  ;; bottom face
                  ((,(+ x width) ,(+ y height) ,(+ z depth))
                   (,x ,(+ y height) ,(+ z depth))
                   (,(+ x width) ,(+ y height) ,z))
                  ((,(+ x width) ,(+ y height) ,z)
                   (,x ,(+ y height) ,z)
                   (,x ,(+ y height) ,(+ z depth)))
                  ;; left face
                  ((,x ,y ,z)
                   (,x ,y ,(+ z depth))
                   (,x ,(+ y height) ,(+ z depth)))
                  ((,x ,y ,z)
                   (,x ,(+ y height) ,(+ z depth))
                   (,x ,(+ y height) ,z))
                  ;; right face
                  ((,(+ x width) ,(+ y height) ,(+ z depth))
                   (,(+ x width) ,y ,z)
                   (,(+ x width) ,(+ y height) ,z))
                  ((,(+ x width) ,(+ y height) ,(+ z depth))
                   (,(+ x width) ,y ,(+ z depth))
                   (,(+ x width) ,y ,z))))))

(define sphere
  (lambda (transforms x y z radius)
    (torus transforms x y z 0 radius)))
(define torus
  (lambda (transforms x y z rad-t rad-c)
    (define circle (make-circle rad-t 0 0 rad-c steps))
    (append
     (map (lambda (triangle)
            (apply draw-triangle
                   (map transforms
                        triangle)))
          (append-map
           (lambda (step)
             (define circle1
               (map (compose (move x y z) (rotate 'y (* 360 (/ step steps))))
                   circle))
             (define circle2
               (map (compose (move x y z) (rotate 'y (* 360 (/ (+ step 1) steps))))
                   circle)) 
             (map list
                 circle1
                 (append (rest circle1) (list (first circle1)))
                 circle2))
           (build-list steps identity))))))
(define line
  (lambda (transforms x0 y0 z0 x1 y1 z1)
    (draw-line (transforms (list x0 y0 z0))
               (transforms (list x1 y1 z1)))))

(define make-circle
  (lambda (x y z radius steps)
    (map
     (lambda (step)
       (define angle (* 2 pi step))
       (list
        (+ x (* radius (cos angle)))
        (+ y (* radius (sin angle)))
        z))
     (build-list (+ steps 1) (lambda (step)
                               (/ step steps))))))

(define draw-triangle
  (lambda (pt0 pt1 pt2)
    (define starts (list pt0 pt1 pt2))
    (define ends (list pt1 pt2 pt0))
    (append-map draw-line starts ends)))

(define draw-line
  (lambda (pt0 pt1)
    (let* ((x0 (first pt0))
           (x1 (first pt1))
           (y0 (second pt0))
           (y1 (second pt1))
           (z0 (third pt0))
           (z1 (third pt1))
           (dx (abs (- x1 x0)))
           (dy (abs (- y1 y0)))
           (pri (cond ((<= dy dx) 'x)
                      ((<= dx dy) 'y)))
           (sec (cond ((eq? pri 'x) 'y)
                      ((eq? pri 'y) 'x)))
           (pri-i (cond ((eq? pri 'x) x0)
                        ((eq? pri 'y) y0)))
           (pri-f (cond ((eq? pri 'x) x1)
                        ((eq? pri 'y) y1)))
           (sec-i (cond ((eq? sec 'x) x0)
                        ((eq? sec 'y) y0)))
           (sec-f (cond ((eq? sec 'x) x1)
                        ((eq? sec 'y) y1)))
           (pri-dir (if (<= pri-i pri-f) 1 -1))
           (sec-dir (if (<= sec-i sec-f) 1 -1))
           (a (* pri-dir 2 (- sec-f sec-i)))
           (b (* sec-dir 2 (- pri-i pri-f)))
           (check-mp? (if (< 0 (* pri-dir sec-dir))
                          (lambda (mp) (> mp 0))
                          (lambda (mp) (< mp 0)))))
      (do ((mp (+ a (/ b 2)) 
               (+ mp
                  a
                  (if (check-mp? mp) b 0)))
           (pri-c pri-i 
                  (+ pri-c 
                     pri-dir))
           (sec-c sec-i 
                  (+ sec-c  
                     (if (check-mp? mp) sec-dir 0)))
           (pts '() 
                (cons (cond ((eq? pri 'x) (list sec-c pri-c z0))
                            ((eq? pri 'y) (list pri-c sec-c z0)))
                      pts)))
          ((= pri-c pri-f)
           (cons (cond ((eq? pri 'x) (list sec-f pri-f z0))
                       ((eq? pri 'y) (list pri-f sec-f z0)))
                 pts))))))
