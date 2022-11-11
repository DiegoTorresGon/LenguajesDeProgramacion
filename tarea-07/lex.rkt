#lang racket

(require racket/function
		 racket/port)
(require "lex-code.rkt")

(module+ test
  (require rackunit
           rackunit/text-ui)

  (port-count-lines-enabled #t))

(struct token (name value beg end)
  #:transparent)

(define reg-nat
  (reg-repeat 1 +inf.0 (char-set "0123456789")))

(define reg-float
  (reg-conc reg-nat "." reg-nat))

(define (lex-number)
  (lambda (src lexeme beg end)
    (token 'number (string->number lexeme) beg end)))  

(define reg-almost-float
  (reg-union (reg-conc "." reg-nat)
             (reg-conc reg-nat ".")))

(define (lex-numeric name)
  (lambda (src lexeme beg end)
    (token name (string->number lexeme) beg end)))

(define (lex-almost-float src lexeme beg end)
  (error 'arith-parse
         (string-append
          "malformed input at line ~v column ~v: unexpected ~v\n"
          "  maybe you forgot a digit before or after the dot?")
         (pos-line beg)
         (pos-col beg)
         lexeme))

(define reg-alphabetic
  (reg-repeat 1 +inf.0 just-alphabetic))

(define reg-id
  (reg-conc reg-alphabetic
            (reg-kleene reg-nat)))

(define reg-ignore
  (reg-repeat 1 +inf.0 just-whitespace))

(define (lex-ignore src lexeme beg end)
  (lex-letrec src))

(define (lex-identifier)
  (lambda (src lexeme beg end)
    (token 'id (string->symbol lexeme) beg end)))

(define (lex-delimiter name)
  (lambda (src lexeme beg end)
    (token name #f beg end)))

(define (lex-terminal term)
  (lambda (src lexeme beg end)
	(token term (string->number lexeme) beg end)))

(define lex-letrec
  (make-lexer
   'letrec
   (lex-rule reg-nat (lex-number))
   (lex-rule reg-float (lex-number))
   ;; terminals
   (lex-rule "-" (lex-terminal '-))
   (lex-rule "=" (lex-terminal '=))
   (lex-rule "zero?" (lex-terminal 'zero?))
   (lex-rule "if" (lex-terminal 'if))
   (lex-rule "then" (lex-terminal 'then))
   (lex-rule "else" (lex-terminal 'else))
   (lex-rule "let" (lex-terminal 'let))
   (lex-rule "in" (lex-terminal 'in))
   (lex-rule "proc" (lex-terminal 'proc))
   (lex-rule "letrec" (lex-terminal 'letrec))
   ;; delimiters
   (lex-rule "(" (lex-delimiter 'opar))
   (lex-rule ")" (lex-delimiter 'cpar))
   (lex-rule "," (lex-delimiter 'comma))
   ;; 
   (lex-rule reg-id (lex-identifier))
   ;; ignorables
   (lex-rule reg-ignore lex-ignore)
   ;; common errors
   (lex-rule reg-almost-float lex-almost-float)))

(define (lex-letrec* src)
  (define t (lex-letrec src))
  (if (eof-object? t)
	null
	(cons t (lex-letrec* src))))

(module+ test
  
  (define (lex str)
    (map (lambda (t)
           (list (token-name t) (token-value t)))
         (lex-letrec* (open-input-string str))))

  (run-tests
   (test-suite
    "letrec lex test"
    (check-equal? (lex "") '())
    (check-equal? (lex "0") '((number  0)))
    (check-equal? (lex "000") '((number 0)))
    (check-equal? (lex "025") '((number 25)))
    (check-equal? (lex "86420") '((number 86420)))
    (check-equal? (lex "3.141592") '((number 3.141592)))
    (check-equal? (lex "0.0") '((number 0.0)))

	(check-equal? (lex "diego") '((id diego)))
	(check-equal? (lex "diego1") '((id diego1)))

	(check-equal? (lex "-(134, 2)") '((- #f)
									 (opar #f)
									 (number 134)
									 (comma #f)
									 (number 2)
									 (cpar #f)))
	(check-equal? (lex "letrec f(x) = 
					      if zero?(x)
						  	 then 0
							 else -(x, -(0, f(-(x, 1))))
						  in (f 10)")
				   '((letrec #f) (id f) (opar #f)
						(id x) (cpar #f) (= #f) (if #f)
						(zero? #f) (opar #f) (id x) (cpar #f)
						(then #f) (number 0) (else #f) (- #f) (opar #f)
						(id x) (comma #f) (- #f) (opar #f) (number 0)
						(comma #f) (id f) (opar #f) (- #f) (opar #f)
						(id x) (comma #f) (number 1) (cpar #f) (cpar #f)
						(cpar #f) (cpar #f) (in #f) (opar #f) (id f)
						(number 10) (cpar #f))))))



    

