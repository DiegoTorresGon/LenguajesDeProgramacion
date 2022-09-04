#lang racket

(define (unit-string? x)
  (and (string? x)
	   (= (string-length x) 1)))

(define (unit-string-list? ls)
  (or (null? ls)
	  (and (pair? ls)
		   (unit-string? (first ls))
		   (unit-string-list? (rest ls)))))

(define (explode s)
  (unless (string? s)
	(error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
	(error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

;This is an alternate definition of bundle that is a bit more complicated than the next one.
;(define (bundle s n)
;  (define (bundle-help s n res last)
;	(cond 
;	  [(null? s) 
;	   (if (equal? last "") res (append res (list last)))]
;	  [(not (< (string-length last) n)) 
;	   	(bundle-help (rest s) n (append res (list last)) (first s))]
;	  [else (bundle-help (rest s) n res (string-append last (first s)))])) 
;  (unless (unit-string-list? s)
;	(error 'bundle "esperaba una lista de cadenas unitarias, pero recibí: ~e" s))
;  (unless (and (integer? n) (> n 0))
;	(error 'bundle "n inválida, recibí: ~e" n))
;  (bundle-help s n '() ""))

(define (take l n)
  (if (or (null? l) (= n 0))
	'()
	(cons (first l) (take (rest l) (- n 1)))))

(define (drop l n)
  (cond 
	[(null? l) '()]
	[(= n 0) l]
  	[else (drop (rest l) (- n 1))]))

(define (bundle s n)
  (unless (unit-string-list? s)
	(error 'bundle "esperaba una lista de cadenas unitarias, pero recibí: ~e" s))
  (unless (and (integer? n) (> n 0))
	(error 'bundle "n inválida, recibí: ~e" n))
  (cond
	[(null? s) null]
	[else
	  (cons (implode (take s n))
			(bundle (drop s n) n))]))

(provide (all-defined-out))
