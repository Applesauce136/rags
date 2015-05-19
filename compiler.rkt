#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(provide get-commands)

;; LEXER
;; ----------------------------------------------------------------
(define-tokens t 
  (NUMBER STRING))

(define-empty-tokens et
  (FRAMES VARY BASENAME
          PUSH POP
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
         ((:or "FRAMES" "VARY" "BASENAME"
               "PUSH" "POP"
               "MOVE" "SCALE" "ROTATE"
               "BOX" "SPHERE" "TORUS" "LINE"
               "SAVE"
               "frames" "vary" "basename"
               "push" "pop"
               "move" "scale" "rotate"
               "box" "sphere" "torus" "line"
               "save")
          (string->symbol (string-upcase lexeme)))
         ((:: (:? #\-)
              (:or (:: (:+ numeric) (:? #\.) (:* numeric))
                   (:: (:* numeric) (:? #\.) (:+ numeric))))
          (token-NUMBER (string->number lexeme)))
         ((:+ (:or alphabetic numeric punctuation symbolic))
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
     ((FRAMES NUMBER)
      `(frames ,$2))
     ((BASENAME STRING)
      `(basename ,$2))
     ((VARY STRING NUMBER NUMBER NUMBER NUMBER)
      `(vary (string->symbol ,$2) ,$3 ,$4 ,$5 ,$6))
     ((PUSH)
      '(push))
     ((POP)
      '(pop))
     ((MOVE NUMBER NUMBER NUMBER)
      `(move ,$2 ,$3 ,$4))
     ((MOVE NUMBER NUMBER NUMBER STRING)
      `(move ,$2 ,$3 ,$4 ,$5))
     ((SCALE NUMBER NUMBER NUMBER)
      `(scale ,$2 ,$3 ,$4))
     ((SCALE NUMBER NUMBER NUMBER STRING)
      `(scale ,$2 ,$3 ,$4 ,$5))
     ((ROTATE STRING NUMBER)
     `(rotate (string->symbol ,$2) ,$3))
     ((ROTATE STRING NUMBER STRING)
     `(rotate (string->symbol ,$2) ,$3 (string->symbol ,$4)))
     ((BOX NUMBER NUMBER NUMBER
           NUMBER NUMBER NUMBER)
      `(box ,$2 ,$3 ,$4 ,$5 ,$6 ,$7))
     ((SPHERE NUMBER NUMBER NUMBER
              NUMBER)
      `(sphere ,$2 ,$3 ,$4 ,$5))
     ((TORUS NUMBER NUMBER NUMBER
             NUMBER NUMBER)
      `(torus ,$2 ,$3 ,$4 ,$5 ,$6))
     ((LINE NUMBER NUMBER NUMBER
            NUMBER NUMBER NUMBER)
      `(line ,$2 ,$3 ,$4 ,$5 ,$6 ,$7))
     ((SAVE STRING)
      `(save ,$2))))))
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
