#lang racket

(require racket/generator)
(provide pad
         number-length)

;; FORMATTING
;; ----------------------------------------------------------------
(define pad
  (lambda (number length)
    (define digits
      (generator
       ()
       (let one-digit ((val number))
         (if (= val 0)
             #f
             (begin (yield (remainder val 10))
                    (one-digit (exact-floor (/ val 10))))))))
    (string-append*
     (reverse
      (for/list ((place-value (build-list length identity)))
        (define one-digit (digits))
        (if one-digit
            (number->string one-digit)
            (number->string 0)))))))

(define number-length
  (lambda (number)
    (if (<= 0  number 9)
        1
        (+ 1 (number-length (exact-floor (/ number 10)))))))
;; ================================================================
