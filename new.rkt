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
               "SAVE")
          (string->symbol lexeme))
         ((:: (:+ numeric) (:? #\.) (:* numeric))
          (token-NUMBER (string->number lexeme)))
         ((:+ (:or alphabetic numeric punctuation))
          (token-STRING lexeme))))

(define my-port (open-input-file "script_c"))

(letrec ((one-line (lambda ()
                     (define token (my-lexer my-port))
                     (printf "~s~n" token)
                     (unless (eq? 'EOF token)
                       (one-line)))))
  (one-line))

;; ================================================================

;; PARSER
;; ----------------------------------------------------------------
(define my-parser
  (parser 
   (start input) 
   (end NEWLINE EOF) 
   (tokens t et)
   (error (lambda (a b c) (void)))
   
   (grammar 
    (input 
     (() #f) 
     ((input command) #f)) 
    (command 
     ((PUSH) #f)
     ((POP) #f)
     ((MOVE NUMBER NUMBER NUMBER) #f)
     ((SCALE NUMBER NUMBER NUMBER) #f)
     ((ROTATE NUMBER NUMBER NUMBER) #f)
     ((BOX NUMBER NUMBER NUMBER) #f)
     ((SPHERE NUMBER) #f)
     ((TORUS NUMBER) #f)
     ((LINE NUMBER NUMBER NUMBER) #f)
     ((SAVE STRING) #f)))))
;; ================================================================
