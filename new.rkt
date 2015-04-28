#lang racket

(require parser-tools/lex)

(define my-lexer
  (lexer ((eof) "goodbye!")
         (any-char lexeme)))

(define my-port (open-input-file "script_c"))

(define-tokens my-tokens (a s d f))

(my-lexer my-port)
(my-lexer my-port)
(my-lexer my-port)
(my-lexer my-port)
(my-lexer my-port)
