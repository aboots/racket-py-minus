#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;scanner
(define scanner
  (lexer
   (";" (token-SEMICOL))
   ("pass" (token-PASS))
   ("break" (token-BREAK))
   ("continue" (token-CONTINUE))
   ("=" (token-ASSIGN))
   ("return" (token-RETURN))
   ("global" (token-GLOBAL))
   ("def" (token-DEF))
   ("(" (token-OPEN-PAR))
   (")" (token-CLOSE-PAR))
   (":" (token-COLON))
   ("," (token-COMMA))
   ("if" (token-IF))
   ("else" (token-ELSE))
   ("for" (token-FOR))
   ("in" (token-IN))
   ("or" (token-OR))
   ("and" (token-AND))
   ("not" (token-NOT))
   ("==" (token-EQUAL))
   ("<" (token-LESS))
   (">" (token-GREATER))
   ("+" (token-PLUS))
   ("-" (token-MINUS))
   ("*" (token-MUL))
   ("/" (token-DIV))
   ("**" (token-POW))
   ("[" (token-OPEN-BRACKET))
   ("]" (token-CLOSE-BRACKET))
   ("()" (token-ZERO-ARG))
   ("True" (token-TRUE))
   ("False" (token-FALSE))
   ("None" (token-NONE))
   ("[]" (token-EMPTY-LIST))
   ("print" (token-PRINT))
   ("int" (token-INT))
   ("float" (token-FLOAT))
   ("bool" (token-BOOL))
   ("list" (token-LIST))
   ("->" (token-RETURNSYMBOL))
   ("checked" (token-CHECKED))
   ((:or
     (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
   ((:: (:or (char-range #\A #\Z) (char-range #\a #\z) #\_) (:* (:or (char-range #\A #\Z) (char-range #\a #\z) #\_ (char-range #\0 #\9))))
    (token-ID (string->symbol lexeme)))
   (whitespace (scanner input-port)) ;*************
   ((eof) (token-EOF))))

;tokens
(define-tokens a (NUM ID))
(define-empty-tokens b (SEMICOL PASS BREAK CONTINUE ASSIGN RETURN
                                GLOBAL DEF OPEN-PAR CLOSE-PAR COLON
                                COMMA IF ELSE FOR IN OR AND NOT EQUAL
                                LESS GREATER PLUS MINUS MUL POW DIV
                                OPEN-BRACKET CLOSE-BRACKET ZERO-ARG
                                TRUE FALSE NONE EMPTY-LIST EOF PRINT
                                INT FLOAT BOOL LIST RETURNSYMBOL CHECKED))
   
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define (lex input) (lex-this scanner (open-input-string input)))
