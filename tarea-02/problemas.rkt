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

(define (list->chunks l n)
  (if (null? l) 
	'()
	(cons (take l n) (list->chunks (drop l n) n)))) 
  
(define (partition s n)
  (unless (and (string? s) (integer? n)
			   (> n 0))
	(error "s o n inválidos"))
  (cond 
	[(= (string-length s) 0) '()]
	[(< (string-length s) n) (cons s '())]
	[else (cons (substring s 0 n) (partition (substring s n) n))]))


(define (isort ls comp)
  (define (insert n ls cmp)
	(cond
	  [(null? ls) (list n)]
	  [(cmp n (car ls)) (cons n ls)]
	  [else (cons (car ls) (insert n (cdr ls) cmp))]))
  (if (null? ls)
	'()
	(insert (car ls)
			(isort (rest ls) comp) 
			comp)))

(define (quicksort ls comp)
  (define (smallers ls piv)
	(filter (lambda (x) (comp x piv)) ls))
  (define (larger-or-eq ls piv)
	(filter (lambda (x) (not (comp x piv))) ls))
  (cond 
	[(null? ls) '()]
	[else
	  (define piv (car ls))
	  (append (quicksort (smallers (cdr ls) piv) comp)
			  (list piv)
			  (quicksort (larger-or-eq (cdr ls) piv) comp))]))

(define (find-time-switch n)
  (define (rand-list elms n)
	(if (= n 0)
	  '()
	  (cons (random elms) (rand-list elms (- n 1)))))
  (let ([ls (rand-list n n)])
	(let ([q-time (second (call-with-values 
					(thunk (time-apply 
							 (lambda (l) (quicksort l <)) (list ls))) list))]
		  [i-time (second (call-with-values 
					(thunk (time-apply 
							 (lambda (l) (isort l <)) (list ls))) list))])
  	(if (> i-time q-time)
	  n
	  (find-time-switch (+ n 1))))))

(define (timing-trials-expct-val n)
  (define (timing-help n)
  	(if (= n 0)
	  0
	  (+ (find-time-switch 630) (timing-help (- n 1)))))
  (round (/ (timing-help n) n)))

(define threshold-quicksort (timing-trials-expct-val 200))

(define (quicksort-opt ls cmp)
  (if (< (length ls) threshold-quicksort)
	(isort ls cmp)
	(quicksort ls cmp)))


(provide (all-defined-out))
