#lang racket

(define vars (shuffle
              (append-map (curry make-list 100)
                   '(l c h b))))

(define randnum
  (lambda (n)
    (+ 1 (random 499))))

(define things (map
                (lambda (var)
                  (list var (build-list
                             (cond ((eq? 'l var)
                                    6)
                                   ((eq? 'c var)
                                    3)
                                   (else
                                    8))
                             randnum)))
                vars))

(define deepmap
  (lambda (proc lst)
    (map (lambda (elt)
           (if (list? elt)
               (deepmap proc elt)
               (proc elt)))
         lst)))

(call-with-output-file "script_c"
  #:exists 'replace
  (lambda (out)
    (deepmap
     (lambda (bit)
       (display bit out)
       (display " " out))
     things)
    (display "g pic.ppm" out)))

