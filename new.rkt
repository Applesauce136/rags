#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

;; LEXER
;; ----------------------------------------------------------------
(define-tokens t 
  (NUMBER STRING))

(define-empty-tokens et
  (PUSH POP
        MOVE SCALE ROTATE
        BOX SPHERE TORUS LINE
        COMMENT SAVE
        EOF NEWLINE))

(define my-lexer
  (lexer ((eof) 'EOF)
         ((:or #\tab #\space)
          (my-lexer input-port))
         (#\newline
          (token-NEWLINE))
         ((:or "PUSH" "POP"
               "MOVE" "SCALE" "ROTATE"
               "BOX" "SPHERE" "TORUS" "LINE"
               "SAVE")
          (string->symbol lexeme))
         ((:: "#" (:* (:~ #\newline)) #\newline)
          (token-COMMENT))
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
;; (define my-parser
;;   (parser 
;;    (start input) 
;;    (end EOF) 
;;    (tokens t et) 
;;    (grammar 
;;     (input 
;;      (() #f) 
;;      ((input command) #f)) 
;;     (command 
;;      ((COMMENT) #f) 
;;      ((LIGHT STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((MOVE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((MOVE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((CONSTANTS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((CONSTANTS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((SAVE_COORDS STRING) #f) 
;;      ((CAMERA DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f)
;;      ((TEXTURE STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((SPHERE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((SPHERE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((SPHERE STRING DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((SPHERE STRING DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((TORUS DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((TORUS DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((TORUS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((TORUS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((BOX DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((BOX DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((BOX STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((BOX STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((LINE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((LINE DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE) #f) 
;;      ((LINE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((LINE DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((LINE STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((LINE STRING DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE) #f) 
;;      ((LINE STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((LINE STRING DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((MESH CO STRING) #f) 
;;      ((MESH STRING CO STRING) #f) 
;;      ((MESH STRING CO STRING STRING) #f) 
;;      ((SET STRING DOUBLE) #f) 
;;      ((SCALE DOUBLE DOUBLE DOUBLE STRING) #f) 
;;      ((SCALE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((ROTATE STRING DOUBLE STRING) #f) 
;;      ((ROTATE STRING DOUBLE) #f) 
;;      ((BASENAME STRING) #f) 
;;      ((SAVE_KNOBS STRING) #f) 
;;      ((TWEEN DOUBLE DOUBLE STRING STRING) #f) 
;;      ((FRAMES DOUBLE) #f) 
;;      ((VARY STRING DOUBLE DOUBLE DOUBLE DOUBLE) #f) 
;;      ((PUSH) #f) 
;;      ((GENERATE_RAYFILES) #f) 
;;      ((POP) #f) 
;;      ((SAVE STRING) #f) 
;;      ((SHADING SHADING_TYPE) #f) 
;;      ((SETKNOBS DOUBLE) #f) 
;;      ((FOCAL DOUBLE) #f) 
;;      ((DISPLAY) #f) 
;;      ((WEB) #f) 
;;      ((AMBIENT DOUBLE DOUBLE DOUBLE) #f)))))
;; ;; ================================================================
