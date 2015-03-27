#lang racket

(provide (contract-out
          (read-script
           (-> string? ; filename
               namespace? ; current namespace (has functions)
               any ; called for side effects
               ))))

(define read-script
  (lambda (filename ns)
    
    (define read-script-helper
      (lambda (in proc)
        (define datum (read in))
        (unless (eof-object? datum)
          (read-script-helper
           in
           (cond ((eq? proc (void))
                  ((curry (eval datum ns))))
                 ((procedure? ((curry proc datum)))
                  (curry proc datum))
                 (else
                  (void)))))))

    (call-with-input-file filename
      (lambda (in)
        (read-script-helper in (void))))))
