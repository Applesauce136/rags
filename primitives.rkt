#lang racket

(provide draw-triangle
         draw-line
         translate-point
         scale-point
         rotate-point)

(define translate-point
  (lambda (tx ty tz pt)
    (list (+ tx (first pt))
          (+ ty (second pt))
          (+ tz (third pt)))))

(define scale-point
  (lambda (sx sy sz pt)
    (list (* sx (first pt))
          (* sy (second pt))
          (* sz (third pt)))))

(define rotate-point
  (lambda (axis angle_d pt)
    (define angle (degrees->radians angle_d))
    (let ((x (first pt))
          (y (second pt))
          (z (third pt)))
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

(define draw-triangle
  (lambda (pt0 pt1 pt2)
    (define starts (list pt0 pt1 pt2))
    (define ends (list pt1 pt2 pt0))
    (append-map draw-line starts ends)))

(define draw-line
  (lambda (pt0 pt1)
    (let* ((y0 (exact-floor (first pt0)))
           (x0 (exact-floor (second pt0)))
           (y1 (exact-floor (first pt1)))
           (x1 (exact-floor (second pt1)))
           (z (exact-floor (third pt0)))
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
                (cons (cond ((eq? pri 'x) (list sec-c pri-c z))
                            ((eq? pri 'y) (list pri-c sec-c z)))
                      pts)))
          ((= pri-c pri-f)
           (cons (cond ((eq? pri 'x) (list sec-f pri-f z))
                       ((eq? pri 'y) (list pri-f sec-f z)))
                 pts))))))
