#lang racket

(require racket/generator

         "transforms.rkt"
         "primitives.rkt")

(provide move-point scale-point rotate-point
         make-box make-sphere make-torus make-triangle make-line)

(define steps 30)

(define make-box
  (lambda (transforms x y z width height depth)
    (define big-gen
      (apply mash-generators
             (map (lambda (triangle)
                    (define transformed (map transforms triangle))
                    (if (frontface? transformed)
                        (apply draw-triangle transformed)
                        (generator () (yield #f))))
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
    (define circle-generator
      (lambda (bigstep init offset)
        (generator
         ()
         (let pixel
             ((step init))
           (if (= step (+ steps init))
               #f
               (begin
                 (yield ((compose transforms
                                  (rotate-point 'y (* 360 (/ (+ bigstep offset) steps))))
                         (list (+ rad-t (* rad-c (cos (* 2 pi (/ step steps)))))
                               (+ 0 (* rad-c (sin (* 2 pi (/ step steps)))))
                               0)))
                 (pixel (+ step 1))))))))
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
                    (circle-generator bigstep 0 0)
                    (circle-generator bigstep 1 0)
                    (circle-generator bigstep 0 1)))
               (lambda (c0 c1 c2)
                 (let one-triangle
                     ((p0 (c0)) (p1 (c1)) (p2 (c2)))
                   (if (and p0 p1 p2)
                       (let ((triangle (draw-triangle p0 p1 p2)))
                         (begin
                           (when (frontface? (list p0 p1 p2))
                             (let one-pt
                                 ((val (triangle)))
                               (if val
                                   (begin (yield val)
                                          (one-pt (triangle)))
                                   #f)))
                           (one-triangle (c0) (c1) (c2))))
                       #f))))
             (one-ring (+ bigstep 1))))))))

(define make-triangle
  (lambda (transforms x0 y0 z0 x1 y1 z1 x2 y2 z2)
    (apply draw-triangle (map transforms `((,x0 ,y0 ,z0)
                                           (,x1 ,y1 ,z1)
                                           (,x2 ,y2 ,z2))))))

(define make-line
  (lambda (transforms x0 y0 z0 x1 y1 z1)
    (draw-line (transforms (list x0 y0 z0))
               (transforms (list x1 y1 z1)))))
