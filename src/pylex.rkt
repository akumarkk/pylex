#lang racket
(require parser-tools/lex-sre)
(require parser-tools/lex)



(define-lex-abbrev hash-comment ("#"))

(define-lex-abbrev operator (or "+"      "-"     "*"     "**"     "/"      "//"    "%" 
                                "<<"     ">>"    "&"     "^"      "|"      "^"     "~"  
                                "<"      ">"     "<="    ">="     "=="     "!="))

(define-lex-abbrev delimiter (or "("     ")"     "["     "]"      "{"      "}" 
                                 ","     ":"     "."     ";"      "@"      "="     "->" 
                                 "+="    "-="    "*="    "/="     "//="    "%="
                                 "&="    "|="    "^="    ">>="    "<<="    "**="))

(define-lex-abbrev keyword (or "False"	   "None"    "True"    "and"	"as" 
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

(define basic-printing-lexer
  (lexer 
   
   [(: operator)
    ; =>
    (begin (display "found an operator  :  ")
           (display lexeme)
           (newline)
           (basic-printing-lexer input-port))]
   
   
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

(define in (open-input-string "+"))
(basic-printing-lexer in)

