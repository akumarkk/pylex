#lang racket
(require parser-tools/lex-sre)
(require parser-tools/lex)

(define-lex-abbrev keyword (or "true" "false"))

(define basic-printing-lexer
  (lexer 
   
   [(complement keyword)
    ; =>
    (begin (display "found a match :  ")
           (display lexeme)
           (newline))]
   
   [(union #\space #\newline)
    ; =>
    (void)]
   
   [(repetition 1 +inf.0 
                (char-range #\a #\z))
    ; =>
    (begin (display "found an id: ")
           (display lexeme)
           (newline))]
   
   [(union #\space #\newline)
    ; =>
    (void)]))

(define (run-basic-printing-lexer port)
  (when (not (eq? 'eof (basic-printing-lexer port)))
    (run-basic-printing-lexer port)))

(run-basic-printing-lexer (open-input-string "foo    bar baz"))
(run-basic-printing-lexer (open-input-string "foo"))
(run-basic-printing-lexer (open-input-string "zoo"))

(define in (open-input-string "true"))
(basic-printing-lexer in)

