#lang racket

(require "../contracts.rkt")

(provide (contract-out
          (make-matrix
           (-> (listof (listof number?))
               matrix/c))
          (matrix-row
           (-> matrix/c exact-nonnegative-integer?
               (vectorof number?)))
          (matrix-col
           (-> matrix/c exact-nonnegative-integer?
               (vectorof number?)))
          (matrix-multiply
           (-> matrix/c matrix/c
               matrix/c))))

(define make-matrix
  (lambda (biglist)
    (list->vector
     (map list->vector biglist))))

(define matrix-row
  (lambda (mtx row)
    (vector-ref mtx row)))

(define matrix-col
  (lambda (mtx col)
    (vector-map
     (curryr vector-ref col)
     mtx)))

(define matrix-rows
  (lambda (mtx)
    (vector-length mtx)))

(define matrix-cols
  (lambda (mtx)
    (vector-length (matrix-row mtx 0))))

(define matrix-ref
  (lambda (mtx row col)
    (vector-ref (matrix-row mtx row) col)))

(define matrix-multiply
  (lambda (mtx1 mtx2)
    (vector-map
     (lambda (mtx-row)
       (vector-map
        (curry dot mtx-row)
        (matrix-rotate mtx2)))
     mtx1)))

(define dot
  (lambda (v1 v2)
    (foldl + 0
           (vector->list (vector-map * v1 v2)))))

(define matrix-rotate
  (lambda (mtx)
     (vector-map (curry matrix-col mtx)
                 (build-vector (matrix-cols mtx) identity))))

;; (define ident (make-matrix '((1 0 0 0)
;;                              (0 1 0 0)
;;                              (0 0 1 0)
;;                              (0 0 0 1))))
;; (define thing (make-matrix '((1 2 3 4)
;;                              (5 6 7 8)
;;                              (9 10 11 12)
;;                              (13 14 15 16))))
;; (matrix-multiply ident thing)
