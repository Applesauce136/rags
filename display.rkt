#lang racket

(provide
 make-image
 set-image-pixel!
 image->string
 write-image!

 make-pixel
 pixel->string
 )

;; IMAGE STUFF
;; ----------------------------------------------------------------
;; pixels is a vector, max is an integer
(struct image (pixels rows cols max))

(define make-image
  (lambda (rows cols (max 255))
    (image (make-vector (* rows cols) 
                        (make-pixel 0 0 0))
           rows cols max)))

(define set-image-pixel!
  (lambda (img pix row col)
    (define 2d->1d (lambda (row col rows)
                     (+ (* rows
                           row)
                        col)))
    (define ref (2d->1d row col (image-rows img)))
    (when (< ref (vector-length (image-pixels img)))
      (vector-set! 
       (image-pixels img)
       ref
       pix))))

(define image->string
  (lambda (img)
    (string-append* "P3 "
		    (number->string (image-rows img))
		    " "
		    (number->string (image-cols img))
		    " "
		    (number->string (image-max img))
		    " "
                    (vector->list (vector-map pixel->string (image-pixels img))))))

(define write-image!
  (lambda (img filename)
    (define out (open-output-file filename
				  #:mode 'text
				  #:exists 'replace))
    (display (image->string img) out)
    (close-output-port out)))
;; ================================================================

;; PIXEL STUFF
;; ----------------------------------------------------------------
(struct pixel (red green blue max))
;; red, green, blue, and max are integers

(define make-pixel
  (lambda (red green blue (max 255))
    (pixel red green blue max)))

(define pixel->string
  (lambda (pix)
    (string-append (number->string (pixel-red pix))
		   " "
		   (number->string (pixel-green pix))
		   " "
		   (number->string (pixel-blue pix))
		   " ")))
;; ================================================================
