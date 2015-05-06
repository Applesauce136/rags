#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

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
     ((PUSH) `(push))
     ((POP) `(pop))
     ((MOVE NUMBER NUMBER NUMBER) `(move ,$2 ,$3 ,$4))
     ((SCALE NUMBER NUMBER NUMBER) `(scale ,$2 ,$3 ,$4))
     ((ROTATE STRING NUMBER) `(rotate ,$2 ,$3))
     ((BOX NUMBER NUMBER NUMBER
           NUMBER NUMBER NUMBER) `(box ,$2 ,$3 ,$4 ,$5 ,$6 ,$7))
     ((SPHERE NUMBER NUMBER NUMBER
              NUMBER) `(sphere  ,$2 ,$3 ,$4 ,$5))
     ((TORUS NUMBER NUMBER NUMBER
             NUMBER NUMBER) `(torus ,$2 ,$3 ,$4 ,$5 ,$6))
     ((LINE NUMBER NUMBER NUMBER
            NUMBER NUMBER NUMBER) `(line ,$2 ,$3 ,$4 ,$5 ,$6 ,$7))
     ((SAVE STRING) `(save ,$2))))))
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
