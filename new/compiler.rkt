#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         racket/draw

         "commands.rkt")

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

(define stack '())
(set! stack (cons identity stack))
(define my-bitmap-dc (new bitmap-dc% (bitmap (make-bitmap 500 500))))
(send my-bitmap-dc set-background (make-color 0 0 0))
(send my-bitmap-dc clear)
(define draw-pixel
  (lambda (pixel)
    (send my-bitmap-dc set-pixel
          (first pixel) (second pixel) (make-color 0 255 255))))

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
      `(set! stack (cons (compose (rotate ,$2 ,$3)
                                  (first stack))
                         (rest stack))))
     ((BOX NUMBER NUMBER NUMBER
           NUMBER NUMBER NUMBER)
      `(map draw-pixel
            (box (first stack) ,$2 ,$3 ,$4 ,$5 ,$6 ,$7)))
     ((SPHERE NUMBER NUMBER NUMBER
              NUMBER)
      `(map draw-pixel
            (sphere (first stack) ,$2 ,$3 ,$4 ,$5)))
     ((TORUS NUMBER NUMBER NUMBER
             NUMBER NUMBER)
      `(map draw-pixel
            (torus (first stack) ,$2 ,$3 ,$4 ,$5 ,$6)))
     ((LINE NUMBER NUMBER NUMBER
            NUMBER NUMBER NUMBER)
      `(map draw-pixel
            (line (first stack) ,$2 ,$3 ,$4 ,$5 ,$6 ,$7)))
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

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(display "Enter the name of your file: ")
(define trash ; so that you don't print it
  (map (curryr eval ns)
       (call-with-input-file (symbol->string (read))
         get-commands)))
