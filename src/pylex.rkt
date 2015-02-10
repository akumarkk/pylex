#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define (output-endmarker? input-port)
  (equal?   input-port))

(define (unget port)
  (file-position port (- (file-position port) 1)))

;count spaces:
(define space-count 0)

(define (inc-space!)
  (set! space-count (+ space-count  1)))

(define (reset-spaces!)
  (set! space-count 0))

; Indent stack to hold indentation information


(define indent-stack '())

;(define current-indent (car indent-stack))

(define (push-indent! spaces) 
  (set! indent-stack (cons spaces indent-stack)))

(define (pop-indent!) 
  (define top (car indent-stack))
  (set! indent-stack (cdr indent-stack)))
  



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

(define-lex-abbrev string-quote (or "'''"       "\""""   "'"    "\"" ))

(define-lex-abbrev keyword (:or "False"	   "None"    "True"    "and"	"as" 
				"assert"   "break"   "class"   "continue"
			        "def"      "del"     "elif"    "else"   "except"
				"finally"  "for"     "from"    "global"	"if" 
				"import"   "in"      "is"	"lamda" "nonlocal" 
				"not"	   "or"      "pass" 	"raise" 
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
(define-lex-abbrev decimalinteger (:or (:: nonzerodigit (:*digit)) (:+ #\0)))
(define-lex-abbrev intpart (:+ digit))
(define-lex-abbrev fraction (:: "." (:+ digit)))
(define-lex-abbrev pointfloat (:or (:: (:? intpart) fraction) (:: intpart ".")))
(define-lex-abbrev exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit)))

(define-lex-abbrev exponentfloat (:: (or intpart floatpart) exponent))

(define-lex-abbrev floatnumber (or pointfloat exponentfloat))

(define-lex-abbrev imagnumber (:: (or floatnumber intpart) (or "j" "J")))

(define basic-printing-lexer
  (lexer 
   
   [(:: operator)
    ; =>
    (begin (display "(PUNCT   ")
           (display lexeme)
           (display ")")
           (newline)
           (reset-spaces!)
           (basic-printing-lexer input-port))]
   
   
     [(:: keyword)
    ; =>
    (begin (display "(KEYWORD ")
           (display lexeme)
           (display ")")
           (newline)
           (reset-spaces!)
           (basic-printing-lexer input-port))]
     
     [#\space
       ;=>
       (begin
                  (inc-space!)
                  (display space-count)
                  (newline)
                  (basic-printing-lexer input-port)
                  )]
    
     
     
     [(:: delimiter)
    ; =>
    (begin (display "(PUNCT ")
           (display lexeme)
           (display ")")
           (newline)
           (reset-spaces!)
           (basic-printing-lexer input-port))]
   
   
   [(repetition 1 +inf.0 
                (char-range #\a #\z))
    ; =>
    (begin (display "found an id: ")
           (display lexeme)
           (newline)
           (reset-spaces!)
           (basic-printing-lexer input-port))]
   
   ;[(union #\space #\newline)
    ; =>
    ;(void)]
   ))

(define (run-basic-printing-lexer port)
  (when (not (eq? 'eof (basic-printing-lexer port)))
    (run-basic-printing-lexer port)))

(run-basic-printing-lexer (open-input-string "zoo"))

(define in (open-input-string "+  abc-;"))
(basic-printing-lexer in)

