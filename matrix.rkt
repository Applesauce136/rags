#lang racket

(provide
 make-matrix
 matrix-cols

 add-matrix-col!
 add-matrix-cols!

 matrix->string
 display-matrix
)

;; MATRIX STUFF
;; ----------------------------------------------------------------
(struct matrix (rows (cols #:mutable)))

(define make-matrix
  (lambda (rows)
    (matrix rows (build-list rows (lambda (n) '())))))

(define add-matrix-col!
  (lambda (mtx col)
    (when (= (length col) 
             (matrix-rows mtx))
      (set-matrix-cols! mtx
                        (map (lambda (val row)
                               (cons val 
                                     row))
                             col
                             (matrix-cols mtx))))))

(define add-matrix-cols!
  (lambda (mtx . cols)
    (when (= (length cols) 
             (matrix-rows mtx))
      (map (lambda (col) 
             (add-matrix-col! mtx col)) 
           (matrix-cols-list cols)))))

(define matrix-multiply
  (lambda (row-mtx col-mtx)
    (matrix-cols-list (matrix-cols col-mtx))))

(define matrix-cols-list
  (lambda (cols)
    (eval `(map (lambda n
                  n)
                ,@(map (lambda (lst)
                         `(reverse (list ,@lst))) 
                       cols)))))

(define matrix->string
  (lambda (mtx)
    (string-join
     (map (lambda (row)
            (string-join
             (map (lambda (n)
                    (number->string n))
                  row)
             " "))
          (matrix-cols mtx))
     "\n" 
     #:after-last "\n")))

(define display-matrix
  (lambda (mtx)
    (display (matrix->string mtx))))
;; ================================================================
