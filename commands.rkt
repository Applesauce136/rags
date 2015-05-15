#lang racket

(require racket/generator)

(provide move scale rotate
         make-box make-sphere make-torus make-line)

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
      (call-with-values (lambda () (apply values pixel))
        (lambda (x y z)
          (map exact-floor
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
                            z))
                     (else
                      (printf "invalid axis: ~s~n" axis)
                      pixel))))))))

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

(define make-box
  (lambda (transforms x y z width height depth)
    (define big-gen
      (apply mash-generators
             (map (lambda (triangle)
                    (apply draw-triangle
                           (map transforms triangle)))
                  `( ;; front face
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
    (generator
     ()
     (let one ((pt (big-gen)))
       (if pt
           (begin (yield pt)
                  (one (big-gen)))
           #f)))))

(define make-sphere
  (lambda (transforms x y z radius)
    (make-torus transforms x y z 0 radius)))
(define make-torus
  (lambda (transforms x y z rad-t rad-c)
    (generator
     ()
     (let one-ring
         ((bigstep 0))
       (if (= bigstep steps)
           #f
           (begin
             (call-with-values
                 (lambda ()
                   (values
                    (generator
                     ()
                     (let pixel
                         ((step 0))
                       (if (= step steps)
                           #f
                           (begin
                             (yield ((compose transforms
                                              (rotate 'y (* 360 (/ bigstep steps))))
                                     (list (+ rad-t (* rad-c (cos (* 2 pi (/ step steps)))))
                                           (+ 0 (* rad-c (sin (* 2 pi (/ step steps)))))
                                           0)))
                             (pixel (+ step 1))))))
                    (generator
                     ()
                     (let pixel
                         ((step 1))
                       (if (= step (+ steps 1))
                           #f
                           (begin
                             (yield ((compose transforms
                                              (rotate 'y (* 360 (/ bigstep steps))))
                                     (list (+ rad-t (* rad-c (cos (* 2 pi (/ step steps)))))
                                           (+ 0 (* rad-c (sin (* 2 pi (/ step steps)))))
                                           0)))
                             (pixel (+ step 1))))))
                    (generator
                     ()
                     (let pixel
                         ((step 0))
                       (if (= step (+ steps 1))
                           #f
                           (begin
                             (yield ((compose transforms
                                              (rotate 'y (* 360 (/ (+ bigstep 1) steps))))
                                     (list (+ rad-t (* rad-c (cos (* 2 pi (/ step steps)))))
                                           (+ 0 (* rad-c (sin (* 2 pi (/ step steps)))))
                                           0)))
                             (pixel (+ step 1))))))))
               (lambda (c0 c1 c2)
                 (let one-triangle
                     ((p0 (c0)) (p1 (c1)) (p2 (c2)))
                   (define triangle (draw-triangle p0 p1 p2))
                   (if (and p0 p1 p2)
                       (begin (let one-pt
                                  ((val (triangle)))
                                (if val
                                    (begin (yield val)
                                           (one-pt (triangle)))
                                    #f))
                              (one-triangle (c0) (c1) (c2)))
                       #f))))
             (one-ring (+ bigstep 1))))))
    ;; (generator
    ;;  ()
    ;;  (define c0 (make-circle 0 0 rad-t rad-c))
    ;;  (define c1 (make-circle 0 0 rad-t rad-c))
    ;;  (define c2
    ;;    (generator
    ;;     ()
    ;;     (define circle (make-circle 0 0 rad-t rad-c))
    ;;     (define rot (rotate y (* 360 (/ 1 steps))))
    ;;     (let one-pt ((val (circle)))
    ;;       (if val
    ;;           (begin (yield val)
    ;;                  (one-pt (rot (circle))))
    ;;           #f))))
    ;;  (c1)
    ;;  (let one-thing ((triangle (draw-triangle (c0) (c1) (c2))))
    ;;    (if triangle
    ;;        (begin
    ;;          (let one-other-thing ((val (triangle)))
    ;;            (if val
    ;;                (begin (yield val)
    ;;                       (one-other-thing (triangle)))
    ;;                #f))
    ;;          (one-thing (draw-triangle (c0) (c1) (c2))))
    ;;        #f))
     
    ;;  ;; (define circle-generator-maker
    ;;  ;;   (lambda (offset)
    ;;  ;;     (generator
    ;;  ;;      ()
    ;;  ;;      (let circle ((step offset))
    ;;  ;;        (if (= step steps)
    ;;  ;;            #f
    ;;  ;;            (begin ((rotate 'y (* 360 (/ step steps))) 
    ;;  ;;                    (yield-all (make-circle x y z rad-c)))
    ;;  ;;                   (circle (+ step 1))))))))
    ;;  ;; (define c0 (circle-generator-maker 0))
    ;;  ;; (define c1 (circle-generator-maker 0))
    ;;  ;; (define c2 (circle-generator-maker 1))
    ;;  ;; (c1)
    ;;  ;; (yield-all (draw-triangle (yield-all c0)
    ;;  ;;                           (yield-all c1)
    ;;  ;;                           (yield-all c2)))

    ;;  ;; (generator
    ;;  ;; ()
     
    ;;  ;; (yield-all circle-generator))
    ;;  ;;     (append
    ;;  ;;      (map (lambda (triangle)
    ;;  ;;             (apply draw-triangle
    ;;  ;;                    (map transforms
    ;;  ;;                         triangle)))
    ;;  ;;           (append-map
    ;;  ;;            (lambda (step)
    ;;  ;;              (define circle1
    ;;  ;;                (map (compose (move x y z) (rotate 'y (* 360 (/ step steps))))
    ;;  ;;                     circle))
    ;;  ;;              (define circle2
    ;;  ;;                (map (compose (move x y z) (rotate 'y (* 360 (/ (+ step 1) steps))))
    ;;  ;;                     circle)) 
    ;;  ;;              (map list
    ;;  ;;                   circle1
    ;;  ;;                   (append (rest circle1) (list (first circle1)))
    ;;  ;;                   circle2))
    ;;  ;;            (build-list steps identity))))
    ;;      )
    ))
(define make-line
  (lambda (transforms x0 y0 z0 x1 y1 z1)
    (draw-line (transforms (list x0 y0 z0))
               (transforms (list x1 y1 z1)))))

(define draw-triangle
  (lambda (pt0 pt1 pt2)
    (mash-generators (draw-line pt0 pt1)
                     (draw-line pt1 pt2)
                     (draw-line pt2 pt0))))

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