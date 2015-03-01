#lang racket

(provide
 draw-line
)
;; MR BIG SHOT
;; ----------------------------------------------------------------
(define draw-line
  (lambda (x0 y0 x1 y1)
    (let* ((dx (abs (- x1 x0)))
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
                (cons (cond ((eq? pri 'x) (list pri-c sec-c))
                            ((eq? pri 'y) (list sec-c pri-c)))
                      pts)))
	  ((= pri-c pri-f)
	   (cons (cond ((eq? pri 'x) (list pri-f sec-f))
                       ((eq? pri 'y) (list sec-f pri-f)))
                 pts))))))
;; ================================================================
