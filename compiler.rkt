#lang racket

(require parser-tools/lex
         parser-tools/yacc
<<<<<<< Updated upstream
         (prefix-in : parser-tools/lex-sre))
=======
         (prefix-in : parser-tools/lex-sre)
         
         racket/generator)
>>>>>>> Stashed changes

(provide get-commands)

;; LEXER
;; ----------------------------------------------------------------
(define-tokens t 
  (NUMBER STRING))

(define-empty-tokens et
<<<<<<< Updated upstream
  (FRAMES VARY BASENAME
          PUSH POP
          MOVE SCALE ROTATE
          BOX SPHERE TORUS LINE
          SAVE
          EOF NEWLINE))
=======
  (VARY
   BASENAME FRAMES
   PUSH POP
   MOVE SCALE ROTATE
   BOX SPHERE TORUS LINE
   SAVE
   EOF NEWLINE))
>>>>>>> Stashed changes

(define my-lexer
  (lexer ((eof) 'EOF)
         ((:or #\tab #\space ; skip whitespace and comments 
               (:: "#" (:* (:~ #\newline)) #\newline))
          (my-lexer input-port))
         (#\newline
          (token-NEWLINE))
<<<<<<< Updated upstream
         ((:or "FRAMES" "VARY" "BASENAME"
=======
         ((:or "VARY" "BASENAME" "FRAMES"
>>>>>>> Stashed changes
               "PUSH" "POP"
               "MOVE" "SCALE" "ROTATE"
               "BOX" "SPHERE" "TORUS" "LINE"
               "SAVE"
<<<<<<< Updated upstream
               "frames" "vary" "basename"
=======
               "vary" "basename" "frames"
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
      `(frames ,$2))
     ((BASENAME STRING)
      `(basename ,$2))
     ((VARY STRING NUMBER NUMBER NUMBER NUMBER)
      `(vary (string->symbol ,$2) ,$3 ,$4 ,$5 ,$6))
=======
      `(set! varys (make-vector ,$2 make-hasheq)))
     ((BASENAME STRING)
      `(set! basename ,$2))
     ((VARY STRING NUMBER NUMBER NUMBER NUMBER)
      `(do ((index ,$3 (+ index 1))
            (value ,$5 (+ value (* (/ (- index ,$3)
                                      (- ,$4 ,$3))
                                   ,$6))))
           ((= index ,$4) (hash-set! (vector-ref varys index)
                                     (string->symbol ,$2) value))
         (hash-set! (vector-ref varys index)
                    (string->symbol ,$2) value)))
>>>>>>> Stashed changes
     ((PUSH)
      '(push))
     ((POP)
      '(pop))
     ((MOVE NUMBER NUMBER NUMBER)
<<<<<<< Updated upstream
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
=======
      `(set! stack (cons (compose (move ,$2 ,$3 ,$4)
                                  (first stack))
                         (rest stack))))
     ((MOVE NUMBER NUMBER NUMBER STRING)
      `(set! stack (cons (compose (apply move (map (curry * (hash-ref current-varys (string->symbol ,$5)))
                                                   (,$2 ,$3 ,$4)))
                                  (first stack))
                         (rest stack))))
     ((SCALE NUMBER NUMBER NUMBER)
      `(set! stack (cons (compose (scale ,$2 ,$3 ,$4)
                                  (first stack))
                         (rest stack))))
     ((SCALE NUMBER NUMBER NUMBER STRING)
      `(set! stack (cons (compose (apply move (map (curry * (hash-ref current-varys (string->symbol ,$5)))
                                                   (,$2 ,$3 ,$4)))
                                  (first stack))
                         (rest stack))))
     ((ROTATE STRING NUMBER)
      `(set! stack (cons (compose (rotate (string->symbol ,$2) ,$3)
                                  (first stack))
                         (rest stack))))
     ((ROTATE STRING NUMBER STRING)
      `(set! stack (cons (compose (rotate (string->symbol ,$2) (* (hash-ref current-varys (string->symbol ,$4)) ,$3))
                                  (first stack))
                         (rest stack))))
>>>>>>> Stashed changes
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
    (generator
     ()
     (let one-match ((result (my-parser
                              (lambda ()
                                (my-lexer in)))))
       (if result
           (begin (yield result)
                  (one-match (my-parser
                              (lambda ()
                                (my-lexer in)))))
           #f)))))
;; ================================================================
