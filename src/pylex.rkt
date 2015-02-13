	    ; Anil Kumar Konasale krishna
	    ;	akumarkk@cs.utah.edu 
	    ;	    UID - u0939372

	    ; Sahana Sandeep
	    ;	sahana@cs.utah.edu
	    ;	    UID - u0932424

#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require (planet dyoo/while-loop))


(define (unget port)
  (file-position port (- (file-position port) 1)))

;count spaces:
(define current-spaces 0)

(define (reset-spaces!)
  (set! current-spaces 0))

(define (inc-space!)
  (set! current-spaces (+ current-spaces 1)))

(define paren-stack '())

(define (push-paren! char)
        (set! paren-stack (cons char paren-stack)))

(define (pop-paren! char)
        (define top (car paren-stack))
        (set! paren-stack (cdr paren-stack))
        (match* {top char}
          [{"(" ")"} (void)]
          [{"[" "]"} (void)]
          [{"{" "}"} (void)]
          [{_     _} (display "mismatched parens")]))

; Indent stack to hold indentation information
(define indent-stack '())

; current-indent points to top of the indent stack
(define (current-indent)  (car indent-stack))

(define (push-indent! spaces)
  (set! indent-stack (cons spaces indent-stack)))

(define (pop-indent!)
  (set! indent-stack (cdr indent-stack)))




;unicode-get() : Reads UnicodeData.txt to map the unicode name to actual unicode 
;TODO : Return error message if the unicode is not present UnicodeData.txt file

(define unicode-data (open-input-file "./UnicodeData.txt"))
(define line 0)
(define parsed-line 0)
(define uname "")
(define ucode "")
(define unicode-code "")


(define (get-unicode unicode-name)
  ;(display "received name-#")
  ;(display unicode-name)
  ;(display "#")
;(while ( not (eof-object? (line (read-line unicode-data))))
(while ( and (set! line (read-line unicode-data)) (not (eof-object? line)))
       (set! parsed-line (regexp-match #rx"^([0-9A-F]+);([^;]*);([^;]*);([^;]*);[^;]*;([^;]*);[^;]*;([^;]*);[^;]*;[^;]*;[^;]*;[^;]*;([^;]*);([^;]*);([^;]*)" line))

             (set! uname (list-ref parsed-line 2))
             (set! ucode (list-ref parsed-line 1))
             (if (equal? uname unicode-name) 
                 (begin
                   (set! unicode-code (string->number  ucode 16))
                   (set! unicode-code (integer->char unicode-code))
                   (set! unicode-code (string unicode-code))
                   ;integer->char () 
                   ;string()
                   (break))
                 (void))
             ))

;measure-sapce!() - decides whether to push current-space into stack or not
; 1. Push only when current-space is greater than top of the stack
; 2. Pop() each item from the stack and compare against current-spaces.
;         If it matches, emit DEDENT and break
;         Else if end of stack is reached, report error() and exit from this function.

(define flag #t)

(define (measure-spaces!)
  (set! flag #t)
  ;(display "CURRENT SPACES#")
  ;(display current-spaces)
  ;(display "TOP OF STACK#")
  ;(display (current-indent))
  ;(newline)
  (cond 
    [(equal? current-spaces (current-indent)) (void)]
    
    [(> current-spaces (current-indent)) (begin 
                                           (display "(INDENT)") 
                                           (newline)
                                           (push-indent! current-spaces)) ]
    
    [(while (and (not (equal? (current-indent) 0)) (equal? flag #t))
           (cond 
             [(= (current-indent) current-spaces)
               (begin 
                    ;(display "(DEDENT)")
                    ;(newline)
                    (set! flag #f))]
               
               [(begin 
                 (pop-indent!) 
                 (display "(DEDENT)")
                 (newline)
                 (if (and (not (equal? (current-indent) current-spaces)) (equal? (current-indent) 0))
                   (begin 
                     (display "INDENTATION ERROR")
                     (newline)
                     )
                   (void)
                 ))]))]))
  
  ;{(if (and (equal? flag #t)    
  ;         (equal? (current-indent) 0)      
  ;        (equal? (current-indent) current-spaces))
  ;    (begin
  ;      (display "(DEDENT)")
  ;      (newline)
  ;      (set! flag #f))
  ;    (void)))
        
; Print all the dedents corresponding to stack entries
(define (print-remaining-dedents)
         (while (not (equal? (current-indent) 0))
                (begin
                  (display "(DEDENT)")
                  (newline)
                  (pop-indent!))))
  


;(define (top-stack-indent)
;  (set! indent-top  (car indent-stack)))


(define-lex-abbrev NEWLINE (:or "#\newline" "\n" "\r" "\r\n"))

(define-lex-abbrev hash-comment ("#"))

(define-lex-abbrev operator (:or "+"      "-"     "*"     "**"     "/"      "//"    "%"
                                "<<"     ">>"    "&"     "^"      "|"      "^"     "~"

                                "<"      ">"     "<="    ">="     "=="     "!="))

(define-lex-abbrev delimiter (:or "("     ")"     "["     "]"      "{"      "}"
                                 ","     ":"     "."     ";"      "@"      "="     "->"
                                 "+="    "-="    "*="    "/="     "//="    "%="
                                 "&="    "|="    "^="    ">>="    "<<="    "**="))

(define-lex-abbrev string-quote (:or  "'''"         "\"\"\""         "'"         "\""))

(define-lex-abbrev one-string-quote (:or "b'''"       "b\"\"\""         "b'"        "b\""
                                      "B'''"       "B\"\"\""         "B'"        "B\""
                                      "u'''"       "u\"\"\""         "u'"        "u\""
                                      "U'''"       "U\"\"\""         "U'"        "U\""))

(define-lex-abbrev raw1-string-quote(:or  "r'''"       "r\"\"\""         "r'"        "r\""
                                           "R'''"       "R\"\"\""         "R'"        "R\""))
                                     
(define-lex-abbrev raw2-string-quote (:or  "br'''"       "br\"\"\""   "br'"    "br\""
                                          "Br'''"       "Br\"\"\""   "Br'"    "Br\""
                                          "BR'''"       "BR\"\"\""   "BR'"    "BR\""
                                          "br'''"       "br\"\"\""   "br'"    "br\""))




(define-lex-abbrev unicode-quote-start (::    "\\"     "N"    "{"))
(define-lex-abbrev unicode-quote-end (:: "}"))

(define-lex-abbrev keyword (:or "Falsespa"    "None"    "True"    "and"    "as"
                                "assert"   "break"   "class"   "continue"
                                "def"      "del"     "elif"    "else"   "except"
                                "finally"  "for"     "from"    "global" "if"
                                "import"   "in"      "is"       "lamda" "nonlocal"
                                "not"      "or"      "pass"     "raise"
                                "return"   "try"     "while"    "with" "yield"))


(define-lex-abbrev nonzerodigit (char-range #\1 #\9))
(define-lex-abbrev digit (char-range #\0 #\9))
(define-lex-abbrev octdigit (char-range #\0 #\7))
(define (octal-digit? char) (error "implement me!"))
(define-lex-abbrev hexdigit (union digit (char-range #\a #\f) (char-range #\A #\F)))
(define (hex-digit? char) (error "implement me!"))
(define-lex-abbrev bindigit (union #\0 #\1))
(define-lex-abbrev octinteger (:: #\0 (:or #\o #\O) (:+ octdigit)))
(define-lex-abbrev hexinteger (:: #\0 (:or #\x #\X) (:+ hexdigit)))
(define-lex-abbrev bininteger (:: #\0 (:or #\b #\B) (:+ bindigit)))
(define-lex-abbrev decimalinteger (:or (:: nonzerodigit (:* digit)) (:+ #\0)))
(define-lex-abbrev intpart (:+ digit))
(define-lex-abbrev fraction (:: "." (:+ digit)))
(define-lex-abbrev pointfloat (:or (:: (:? intpart) fraction) (:: intpart ".")))
(define-lex-abbrev exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit)))

(define-lex-abbrev exponentfloat (:: (:or intpart pointfloat) exponent))

(define-lex-abbrev floatnumber (:or pointfloat exponentfloat))

(define-lex-abbrev imagnumber (:: (:or floatnumber intpart) (:or "j" "J")))


                     
; string-lexer() : String literal lexer.
; When ever quotes are encountered in basic-lexer, this is invoked.
; It finds the end of the string and displays the string literal.
; There are two types of string :
;                   a. Raw String - escape each "\" in string literal 
;                   b. Usual String literal - Replace unicode names with corresponding characters
(define quote-char 0)
(define raw-string-flag 0)
(define escape-char "\\")
(define unicode-parsing-flag 0)
(define parsed-unicode-name "")
(define app-string "")

(define other-id-start-chars '(#\_ #\u2118 #\u212E #\u309B #\u309C ))
(define other-id-continue-chars '(#\u00B7 #\u0387 #\u1369 #\u1370 #\u1371 #\u19DA))
(define (other-id-start? char) (not (false? (member char other-id-start-chars))))
(define (other-id-continue? char) (not (false? (member char other-id-continue-chars))))
(define (id-start? char) (or (not (false? (member (char-general-category char) '(lu ll lt lm lo nl))))
                             (other-id-start? char)))
(define (id-continue? char) (or (id-start? char)
                                (not (false? (member (char-general-category char) '(mn mc nd pc))))
                                (other-id-continue? char)))
(define (xid-start? char) (and (id-start? char)
                               (id-start? (string-ref (string-normalize-nfkc (string char)) 0))
                               (xid-continue? (car (string->list (string-normalize-nfkc (string char)))))))
                       
(define (xid-continue? char) (and (id-continue? char)
                                  (id-continue? (car (string->list (string-normalize-nfkc (string char)))))))
;to store the identifier string
(define id-string "")

;flag to check if first char of indentifier
(define first-char 0)

;display identifier
(define (emit-id) (cond 
                    [(and (> (string-length id-string) 0) (xid-start? (string-ref id-string 0)))
                                   (begin
                                     (display "(ID \"")
                                     (display id-string)
                                     (display "\")")
                                     (newline)
                                     (set! first-char 0)
                                     (set! id-string ""))]))
                   
(define (id-lexer id-char port) 
                       (cond
                          [(and (xid-start? id-char) (equal? first-char 0))
                           (begin
                             (set! id-string (string-append id-string (string id-char)))
                             (set! first-char 1)
                             )]
                          
                          [(xid-continue? id-char) 
                           (begin
                             (set! id-string (string-append id-string (string id-char)))
                             )]
                                                   
                          ))

;To display punct lexemes
(define (emit-punct str)
  (begin
    (display "(PUNCT \"")
    (display str)
    (display "\")")
    (newline)))

;String-lexer
(define string-lexer
  (lexer
   [ (:+ string-quote) 
       ;=>
       (begin
         (cond
           [(not (equal? lexeme quote-char)) 
            (begin 
              (if (and (equal? lexeme escape-char) (equal? raw-string-flag 1)) 
                  (begin 
                    (string-append lexeme escape-char)) 
                  (void))
              (display lexeme)
              (string-lexer input-port ))]
           
           [ (equal? quote-char lexeme) 
             (begin
               (display "\")")
               (newline)
               (basic-printing-lexer input-port ))]))]
   
           [(:: "\\" NEWLINE)
            (string-lexer input-port)]
   
   ;Handle Unicode characters
   ;[(::(and ("\\N{" unicode-name "}")))
   [(:+ unicode-quote-start)
    ;=>
    (begin
      (set! unicode-parsing-flag 1)
      (string-lexer input-port))]
      
    [(:+ unicode-quote-end)
    ;=>
    (begin
      (cond
        [(equal? unicode-parsing-flag 1) 
         (begin
           (get-unicode parsed-unicode-name)
           (display unicode-code)
           (set! unicode-code "")
           (set! unicode-parsing-flag 0) 
           (string-lexer input-port))]
        
        [(begin
           (display lexeme)
           (string-lexer input-port))]))]      
   
   [any-char 
    ;=>
    (begin
      (cond
        [(and (equal? lexeme escape-char) (equal? raw-string-flag 1))
          (begin
            (display lexeme)
            ;Not sure why string-append is not working here
            ;(string-append lexeme escape-char) 
            )]
        
        ;This char is part of unicode name if unicode-parsing-flag is set
        [(equal? unicode-parsing-flag 1) 
         (begin 
           (set! parsed-unicode-name (string-append parsed-unicode-name lexeme))
           ;(display parsed-unicode-name)
           ;Here Setting lexeme to null because we dont want it to display anything
           ;Other ways of handling involves complicated conditional checks
           (set! lexeme ""))])
      (display lexeme)
      (string-lexer input-port))]))
      
   
; Indentation lexer
(define indentation-lexer
  (lexer
   [#\space
       ;=>
       (begin
                  (inc-space!)
                  ;(display current-spaces)
                  ;(newline)
                  (indentation-lexer input-port)
                  )]
   [(eof)
    ;=>
    (begin
      (print-remaining-dedents)
      (display "(ENDMARKER)")
      (newline))]
   
   [ any-char 
    ;=>
    (begin
      (measure-spaces!)
      (reset-spaces!)
      (unget input-port)
      (basic-printing-lexer input-port))
    ]))

;comment lexer
(define comment-lexer
  (lexer
   
   [any-char
    ;=>
    (comment-lexer input-port)]
   
   [(:+ NEWLINE)
    ;=>
    (basic-printing-lexer input-port)]))

; This is the main lexer. It handles all kinds of python constructs  
(define basic-printing-lexer
  (lexer
   
   [(:+ string-quote)
        ;=>
        (begin
          (emit-id)
          (display "(LIT \"")
          (set! quote-char lexeme)
          (set! raw-string-flag 0)
          (string-lexer input-port))]
      
   [(:or #\( #\{ #\[)
        ;=>
        (begin
          (emit-id)
          (push-paren! lexeme)
          (emit-punct lexeme)
          (basic-printing-lexer input-port))]
   
   [(:or #\) #\} #\])
        ;=>
        (begin
          (emit-id)
          (pop-paren! lexeme)
          (emit-punct lexeme)
          (basic-printing-lexer input-port))]
   
   [(:or decimalinteger hexinteger octinteger bininteger floatnumber imagnumber) 
    ;=>
    (begin 
      (cond
        
        ; When Identifier name contains numerical literals, id-lexer has to be 
        ;   invoked to handle that. Else numeric part of identifier will be 
        ;   treated as integer literal
        [(> first-char 0) 
         (begin
           (id-lexer (string-ref lexeme 0) input-port)
           (basic-printing-lexer input-port))]
        
        [(begin
           (display "(LIT \"")
           (display lexeme)
           (display "\")")
           (newline)
           (basic-printing-lexer input-port ))]))]


   ; Treat normally. u or U has no special meaning in Python3 where as in
   ; Python2 it was required to interpret unicode characters. Otherwise string
   ; is treated as raw string in python
   [(:+ one-string-quote)
    ;=>
    (begin 
      (emit-id)
      (display "(LIT \"")
      (display "matched#")
      (display lexeme)
      (newline)
      (set! quote-char (substring lexeme 1))
      (set! raw-string-flag 0)
      (string-lexer input-port ))]
   
   [(:+ raw1-string-quote) 
    ;=>
    (begin 
      (emit-id)
      (display "(LIT \"")
      (set! quote-char (substring lexeme 1))
      (set! raw-string-flag 1)
      (string-lexer input-port ))]
    
     
   [(:+ raw2-string-quote)
    ;=>
    (begin 
      (emit-id)
      (display "(LIT \"")
      ;lexeme will always holds matched string/pattern. In this case any of rB and quote.
      ;So we need to store only quote not the prefixstring in quote-char.
      ; This can be achieved by taking substring
      (set! quote-char (substring lexeme 2))
      (set! raw-string-flag 1)
      (string-lexer input-port ))]

   [(:+ operator)
    ; =>
    (begin 
      (emit-id)
      (emit-punct lexeme)
      (basic-printing-lexer input-port))]

   ["#" 
    ;=>
    (comment-lexer input-port)]

     [(:: keyword #\space)
    ; =>
    (begin 
      (emit-id)
      (display "(KEYWORD ")
      (display lexeme)
      (display ")")
      (newline)
      (basic-printing-lexer input-port))]
     
     ;explicit line joining
     [(:: "\\" NEWLINE)
      ;=>
      (basic-printing-lexer input-port)]
     
     [(:+ NEWLINE)
      ;=>
      (begin
       (emit-id)
       (display "(NEWLINE)")
       (newline)
       (indentation-lexer input-port))]

     [(:: delimiter)
    ; =>
    (begin 
      (emit-id)
      (emit-punct lexeme)
      (basic-printing-lexer input-port))]
    
     [#\space
    ;=>
      (begin
        (emit-id)
        (basic-printing-lexer input-port))]

     [any-char
    ;=>
    (begin
      (id-lexer (string-ref lexeme 0) input-port)
      (basic-printing-lexer input-port))]

   ;[(repetition 1 +inf.0
    ;            (:or
     ;           (char-range #\a #\z)
      ;          (char-range #\A #\Z))
       ;         )
    ; =>
    ;(begin (display "FOUND AN ID: ")
     ;      (display lexeme)
      ;     (newline)
       ;    (basic-printing-lexer input-port))]
  
   ))

(define (run-basic-printing-lexer port)
  (push-indent! current-spaces)
  (when (not (eq? 'eof (basic-printing-lexer port)))
    (run-basic-printing-lexer port)))
(run-basic-printing-lexer (open-input-file "test.py"))

;(define in (open-input-file "test.py"))
;(basic-printing-lexer in)

	    ; Anil Kumar Konasale krishna
	    ;	akumarkk@cs.utah.edu 
	    ;	    UID - u0939372


            ;	  Sahana Sandeep
            ;   sahana@cs.utah.edu
            ;     UID - u0932424
