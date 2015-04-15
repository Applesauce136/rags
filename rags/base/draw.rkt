#lang racket

(require "../contracts.rkt")

(provide (contract-out
          (draw-triangles
           (-> color/c (listof triangle/c)
               (listof pixel/c)))
          (draw-triangle
           (-> color/c triangle/c
               (listof pixel/c)))
          (draw-lines
           (-> color/c (listof point/c)
               (listof pixel/c))) 
          (draw-line
           (-> color/c point/c point/c
               (listof pixel/c)))))

(define draw-triangles
  (lambda (color tris)
    (append-map (curry draw-triangle color)
                tris)))

(define draw-triangle
  (lambda (color tri)
    (append (draw-lines color tri)
            (draw-line color
                       (first tri)
                       (third tri)))))

(define draw-lines
  (lambda (color pts)
    (append-map (curry draw-line color)
                (take pts (sub1 (length pts)))
                (rest pts))))

(define draw-line
  (lambda (color pt0 pt1)
    (let* ((y0 (exact-floor (first pt0)))
           (x0 (exact-floor (second pt0)))
           (y1 (exact-floor (first pt1)))
           (x1 (exact-floor (second pt1)))
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
                (cons (cond ((eq? pri 'x) (list pri-c sec-c color))
                            ((eq? pri 'y) (list sec-c pri-c color)))
                      pts)))
	  ((= pri-c pri-f)
	   (cons (cond ((eq? pri 'x) (list pri-f sec-f color))
                       ((eq? pri 'y) (list sec-f pri-f color)))
                 pts))))))
