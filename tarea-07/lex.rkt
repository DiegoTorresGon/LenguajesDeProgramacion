#lang racket

(require racket/function
		 racket/port)
(require "./reglex/lex/lex.rkt")

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

(define reg-number
  (reg-union (reg-nat)
			 (reg-float)))

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

(define reg-ignore
  (reg-repeat 1 +inf.0 just-whitespace))

(define (lex-ignore src lexeme beg end)
  (lex-letrec src))

(define (lex-delimiter name)
  (lambda (src lexeme beg end)
    (token name #f beg end)))

(define lex-letrec
  (make-lexer
   'letrec
   (lex-rule reg-number (lex-numeric 'num))
   ;; delimiters
   (lex-rule "(" (lex-delimiter 'opar))
   (lex-rule ")" (lex-delimiter 'cpar))
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
    (check-equal? (lex "0") '((num  0)))
    (check-equal? (lex "000") '((num 0)))
    (check-equal? (lex "025") '((num 25)))
    (check-equal? (lex "86420") '((num 86420)))
    (check-equal? (lex "3.141592") '((num 3.141592)))
    (check-equal? (lex "0.0") '((num 0.0))))))
    

