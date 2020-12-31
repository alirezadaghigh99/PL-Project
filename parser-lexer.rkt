#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require (except-in eopl #%module-begin))

(define lexer_compiler
  (lexer
   ["+" (token-plus)]
   ["-" (token-minus)]
   ["*" (token-mul)]
   ["/" (token-div)]
   [">" (token-gt)]
   ["<" (token-lt)]
   ["<=" (token-lte)]
   [">=" (token-gte)]
   ["==" (token-eq)]
   ["=" (token-seteq)]
   
   ["!=" (token-noteq)]
   [";" (token-semicolon)]
   ["," (token-comma)]
   ["[" (token-lbrack)]
   ["]" (token-rbrack)]
   ["(" (token-lpar)]
   [")" (token-rpar)]
   ["{" (token-lcurly)]
   ["}" (token-rcurly)]
   ["return" (token-ret)]
   ["while" (token-while)]
   ["do" (token-dot)]
   ["end" (token-endwhile)]
   ["if" (token-if)]
   ["then" (token-then)]
   ["else" (token-else)]
   ["null" (token-null null)]
   ["endif" (token-eif)]
   ["func" (token-func)]
   [(:or "true" "false") (token-bool (if (equal? lexeme "true") #t #f))]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) (token-var lexeme)]
   [(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-num (string->number lexeme))]
   [(:: "'" (complement (:: any-string "'" any-string)) "'") (token-string (substring lexeme 1 (- (string-length lexeme) 1)))]
   [whitespace (lexer_compiler input-port)]
   [(eof) (token-eof)]
   [(:+ (:or "@" "!" "#" "%" "^" "&" "~" "{" "}")) (eopl:error (string-append lexeme " is unknown!"))]
  
   ))

(define parser_compiler
  (parser
   (start command)
            (end eof)
            (error (lambda (tok-ok? tok-name tok-value)
                    (displayln tok-ok?)
                     (displayln tok-name)
                     (displayln tok-value)
                     ;(displayln (error-thrower tok-name tok-value))

                    ))
            (tokens a b)
            (grammar

             [command ((rcommand) (list 'rcommand $1)) ((rcommand semicolon command) (list 'lcommand $1 $3))]
             
             [whilecommand ((while exp dot command endwhile) (list 'while $2 $4))]
             
             [rcommand
            ((whilecommand) (list 'whilecommand $1))
            ((ifcommand) (list 'ifcommand $1))
            ((assign) (list 'assigncommand $1))
            ((return) (list 'returncommand $1))
            ]
             
             [ifcommand ((if exp then command else command eif) (list 'if $2 $4 $6))]

             [FUNCTION ((func lpar variables rpar lcurly command rcurly) (list 'function $3 $6))]
           
           [CALL ((var lpar args rpar) (list 'call-function $1 $3))]
           
           [assign
            ((var seteq exp) (list 'seteq $1 $3))
            ]
           
           [return ((ret exp) (list 'return $2))]
           
           [exp
            ((aexp) (list 'aexp $1))
            ((aexp gt aexp) (list 'greater? $1 $3))
            ((aexp lt aexp) (list 'less? $1 $3))
            ((aexp eq aexp) (list 'eqs? $1 $3))
            ((aexp noteq aexp) (list 'noteqs? $1 $3))
            ]
           
           [aexp
            ((bexp) (list 'bexp $1))
            ((bexp minus aexp) (list 'subtract $1 $3))
            ((bexp plus aexp) (list 'add $1 $3))
            ]
           
           [bexp
            ((cexp) (list 'cexp $1))
            ((cexp mul bexp) (list 'mult $1 $3))
            ((cexp div bexp) (list 'div $1 $3))
            ]
      
            [cexp
            ((minus cexp) (list 'minus $2))
            ((lpar exp rpar) (list 'expinpar $2))
            ((num) (list 'number $1))
            ((null) (list 'null $1))
            ((bool) (list 'bool $1))
            ((string) (list 'string $1))
            ((var) (list 'var $1))
            ((list) (list 'list $1))
            ((var listmember) (list 'cflist $1 $2))
            ]
            
            [list
             ((lbrack listValues rbrack) (list 'listvalues $2))
             ((lbrack rbrack) (list 'emptylist))
             ]
            
            [listValues
             ((exp) (list 'lval $1))
             ((exp comma listValues) (list 'lvals $1 $3))
             ]

            [variables
             ((var) (list 'funcvar $1))
             ((var comma variables) (list 'funcvars $1 $3))
             ]
            
            [args
             ((exp) (list 'arg $1))
             ((exp comma args) (list 'args $1 $3))
             ]
            
            [listmember
             ((lbrack exp rbrack) (list 'listmember $2))
             ((lbrack exp rbrack listmember) (list 'listmembers $2 $4))
             ]
           
             )))



(define-tokens a (num string bool var null))
(define-empty-tokens b (eof plus minus mul div seteq eq noteq gt lt gte lte semicolon comma lbrack rbrack lpar rpar ret while dot endwhile if then else eif func lcurly rcurly))

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define lexing (lex-this lexer_compiler (open-input-string "a = 3; b = [2, 3, 4]; return a + b")))

(let ((parser-res (parser_compiler lexing))) parser-res)
(provide (all-defined-out))