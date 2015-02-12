#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require (planet dyoo/while-loop))

(define (output-endmarker? input-port)
  (equal?   input-port))

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
          [{_     _} (error "mismatched parens")]))

; Indent stack to hold indentation information
(define indent-stack '())

; current-indent points to top of the indent stack
(define (current-indent)  (car indent-stack))

(define (push-indent! spaces)
  (set! indent-stack (cons spaces indent-stack)))

(define (pop-indent!)
  (set! indent-stack (cdr indent-stack)))

(define (handle-dedent!)
  (pop-indent!)
  (if (= current-spaces (current-indent)) (display "DEDENT")
      (if (> (current-indent) current-spaces) (display "INDENTATION ERROR") (handle-dedent!))))

;measure-sapce!() - decides whether to push current-space into stack or not
; 1. Push only when current-space is greater than top of the stack
; 2. Pop() each item from the stack and compare against current-spaces.
;         If it matches, emit DEDENT and break
;         Else if end of stack is reached, report error() and exit from this function.

(define flag #t)

(define (measure-spaces!)
  (set! flag #t)
 
  (cond 
    [(equal? current-spaces current-indent) (void)]
    
    [(> current-spaces (current-indent)) (begin 
                                           (display "(INDENT)") 
                                           (newline)
                                           (push-indent! current-spaces)) ]
    
    [(while (and (not (equal? (current-indent) 0)) (equal? flag #t))
           (cond 
             [(= (current-indent) current-spaces)
               (begin 
                    (display "(DEDENT)")
                    (newline)
                    (set! flag #f))]
               
               [(begin 
                 (pop-indent!) 
                 (if (and (not (equal? (current-indent) current-spaces)) (equal? (current-indent) 0))
                   (begin 
                     (display "INDENTATION ERROR")
                     (newline)
                     )
                   (void)
                 )
               )]
           )
     )]
  ))
  



;(define (top-stack-indent)
;  (set! indent-top  (car indent-stack)))


(define-lex-abbrev NEWLINE (:or "#\newline" "\n"))

(define-lex-abbrev hash-comment ("#"))

(define-lex-abbrev operator (:or "+"      "-"     "*"     "**"     "/"      "//"    "%"
                                "<<"     ">>"    "&"     "^"      "|"      "^"     "~"

                                "<"      ">"     "<="    ">="     "=="     "!="))

(define-lex-abbrev delimiter (:or "("     ")"     "["     "]"      "{"      "}"
                                 ","     ":"     "."     ";"      "@"      "="     "->"
                                 "+="    "-="    "*="    "/="     "//="    "%="
                                 "&="    "|="    "^="    ">>="    "<<="    "**="))

(define-lex-abbrev string-quote (:or "'''"       "\""""   "'"    "\"" ))

(define-lex-abbrev keyword (:or "False"    "None"    "True"    "and"    "as"
                                "assert"   "break"   "class"   "continue"
                                "def"      "del"     "elif"    "else"   "except"
                                "finally"  "for"     "from"    "global" "if"
                                "import"   "in"      "is"       "lamda" "nonlocal"
                                "not"      "or"      "pass"     "raise"
                                "return"   "try"     "while"    "with" "yield"))

;String leteral definition
(define-lex-abbrev stringliteral (:: stringprefix (or shortstring longstring)))
(define-lex-abbrev stringprefix  (:or ""r"" ""u"" ""R"" ""U""))
;(define-lex-abbrev shortstring   (:: (:or "'" '"') shortstringitem* (:or "'" '"')))
;(define-lex-abbrev longstring   (:or "'" longstringitem* "'"  '"'longstringitem'"'))
(define-lex-abbrev shortstringitem (:or shortstringchar stringescapeseq))
(define-lex-abbrev longstringitem (:or longstringchar stringescapeseq))
(define-lex-abbrev shortstringchar ())
(define-lex-abbrev longstringchar ())
(define-lex-abbrev stringescapeseq ())




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
(define-lex-abbrev decimalinteger (:or (:: nonzerodigit (:*digit)) (:+ #\0)))
(define-lex-abbrev intpart (:+ digit))
(define-lex-abbrev fraction (:: "." (:+ digit)))
(define-lex-abbrev pointfloat (:or (:: (:? intpart) fraction) (:: intpart ".")))
(define-lex-abbrev exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit)))

(define-lex-abbrev exponentfloat (:: (or intpart floatpart) exponent))

(define-lex-abbrev floatnumber (or pointfloat exponentfloat))

(define-lex-abbrev imagnumber (:: (or floatnumber intpart) (or "j" "J")))


(define quote-char 0)

; String literal lexer
(define string-lexer
  (lexer
   [ (:+ string-quote) 
       ;=>
       (begin
         (cond
           [(not (equal? lexeme quote-char)) 
            (begin 
              (display lexeme)
              (string-lexer input-port ))]
           
           [ (equal? quote-char lexeme) 
             (begin
               (display ")")
               (newline)
               (basic-printing-lexer input-port ))]))]
   
   [any-char 
    ;=>
    (begin
      (display lexeme)
      (string-lexer input-port))]))
      
   
   
(define indentation-lexer
  (lexer
   [#\space
       ;=>
       (begin
                  (inc-space!)
                  (display current-spaces)
                  (newline)
                  (indentation-lexer input-port)
                  )]
   [ any-char 
    ;=>
    (begin
      (measure-spaces!)
      (reset-spaces!)
      (unget input-port)
      (basic-printing-lexer input-port))
    ]))
  
  (define basic-printing-lexer
  (lexer
   
   [(:or #\( #\{ #\[)
        ;=>
        (begin
          (push-paren! lexeme)
          (basic-printing-lexer input-port))]
   
   [(:or #\) #\} #\])
        ;=>
        (begin
          
          (pop-paren! lexeme)
          
          (basic-printing-lexer input-port))]
   
   [(:+ string-quote)
        ;=>
        (begin
          (display "(LIT ")
          (set! quote-char lexeme)
          (string-lexer input-port ))]
         

   [(:: operator)
    ; =>
    (begin (display "(PUNCT   ")
           (display lexeme)
           (display ")")
           (newline)
           (basic-printing-lexer input-port))]


     [(:: keyword)
    ; =>
    (begin (display "(KEYWORD ")
           (display lexeme)
           (display ")")
           (newline)
           (basic-printing-lexer input-port))]
     
     [(:+ NEWLINE)
      ;=>
      (indentation-lexer input-port)]

     [(:: delimiter)
    ; =>
    (begin (display "(PUNCT ")
           (display lexeme)
           (display ")")
           (newline)
           (basic-printing-lexer input-port))]


   [(repetition 1 +inf.0
                (:or
                (char-range #\a #\z)
                (char-range #\A #\Z))
                )
    ; =>
    (begin (display "FOUND AN ID: ")
           (display lexeme)
           (newline)
           (basic-printing-lexer input-port))]

   [#\space
    ; =>
    (basic-printing-lexer input-port)]
   ))

(define (run-basic-printing-lexer port)
  (push-indent! current-spaces)
  (when (not (eq? 'eof (basic-printing-lexer port)))
    (run-basic-printing-lexer port)))
(run-basic-printing-lexer (open-input-string "zoo"))

(define in (open-input-string "+  a     bc          -
     utah;
        university
     BANGALORE
     school
\"Hello\"         (pqrs} 'Hi'
ide
 '''MNO'''
        another id"))
(basic-printing-lexer in)


