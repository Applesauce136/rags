#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         racket/draw)

(provide get-commands)

;; LEXER
;; ----------------------------------------------------------------
(define-tokens t 
  (NUMBER STRING))

(define-empty-tokens et
  (PUSH
   POP
   MOVE SCALE ROTATE
   BOX SPHERE TORUS LINE
   SAVE
   EOF NEWLINE))

(define my-lexer
  (lexer ((eof) 'EOF)
         ((:or #\tab #\space ; skip whitespace and comments 
               (:: "#" (:* (:~ #\newline)) #\newline))
          (my-lexer input-port))
         (#\newline
          (token-NEWLINE))
         ((:or "PUSH" "POP"
               "MOVE" "SCALE" "ROTATE"
               "BOX" "SPHERE" "TORUS" "LINE"
               "SAVE"
               "push" "pop"
               "move" "scale" "rotate"
               "box" "sphere" "torus" "line"
               "save")
          (string->symbol (string-upcase lexeme)))
         ((:: (:? #\-)
              (:or (:: (:+ numeric) (:? #\.) (:* numeric))
                   (:: (:* numeric) (:? #\.) (:+ numeric))))
          (token-NUMBER (string->number lexeme)))
         ((:+ (:or alphabetic numeric punctuation))
          (token-STRING lexeme))))

;; ================================================================

;; PARSER
;; ----------------------------------------------------------------

(define my-parser
  (parser 
   (start start) 
   (end NEWLINE EOF) 
   (tokens t et)
   (error (lambda (a b c) (void)))
   
   (grammar
    
    (start
     (() #f)
     ((error start) $2)
     ((command) $1))
    
    (command
     ((PUSH)
      `(set! stack
         (cons (first stack)
               stack)))
     ((POP)
      `(set! stack (rest stack)))
     ((MOVE NUMBER NUMBER NUMBER)
      `(set! stack (cons (compose (move ,$2 ,$3 ,$4)
                                  (first stack))
                         (rest stack))))
     ((SCALE NUMBER NUMBER NUMBER)
      `(set! stack (cons (compose (scale ,$2 ,$3 ,$4)
                                  (first stack))
                         (rest stack))))
     ((ROTATE STRING NUMBER)
      `(set! stack (cons (compose (rotate (string->symbol ,$2) ,$3)
                                  (first stack))
                         (rest stack))))
     ((BOX NUMBER NUMBER NUMBER
           NUMBER NUMBER NUMBER)
      `(draw-pixels
        (make-box (first stack) ,$2 ,$3 ,$4 ,$5 ,$6 ,$7)))
     ((SPHERE NUMBER NUMBER NUMBER
              NUMBER)
      `(draw-pixels
        (make-sphere (first stack) ,$2 ,$3 ,$4 ,$5)))
     ((TORUS NUMBER NUMBER NUMBER
             NUMBER NUMBER)
      `(draw-pixels
        (make-torus (first stack) ,$2 ,$3 ,$4 ,$5 ,$6)))
     ((LINE NUMBER NUMBER NUMBER
            NUMBER NUMBER NUMBER)
      `(draw-pixels
        (make-line (first stack) ,$2 ,$3 ,$4 ,$5 ,$6 ,$7)))
     ((SAVE STRING)
      `(send (send my-bitmap-dc get-bitmap)
             save-file ,$2 'png))))))
;; ================================================================

;; RUN
;; ----------------------------------------------------------------
(define get-commands
  (lambda (in)
    (let ((result (my-parser
                   (lambda ()
                     (my-lexer in)))))
      (if result
          (cons result (get-commands in))
          '()))))
;; ================================================================
