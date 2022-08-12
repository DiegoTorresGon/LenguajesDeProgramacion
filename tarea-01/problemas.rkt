#lang racket
 
;; Escribe aquí tus soluciones

(define (countdown n)
    (if (= n -1)
	'()
	(cons n (countdown (- n 1)))))

(define (insertL x y ls)
  (cond 
    ((empty? ls) '())
    ((eqv? (car ls) x) (cons y (cons x (insertL x y (cdr ls)))))
    (else (cons (car ls) (insertL x y (cdr ls))))))

(define (remv-1st x ls)
  (cond
    ((null? ls) '())
    ((eqv? (car ls) x) (cdr ls))
    (else (cons (car ls) (remv-1st x (cdr ls))))))

(define (map proc ls)
  (if (null? ls)
      '()
      (cons (proc (car ls)) (map proc (cdr ls)))))

(define (filter pred ls)
  (cond 
    ((null? ls) '())
    ((pred (car ls)) (cons (car ls) (filter pred (cdr ls))))
    (else (filter pred (cdr ls)))))  
	
(define (zip a b)
  (cond
    ((or (null? a) (null? b)) '())
    (else (cons (cons (car a) (car b)) (zip (cdr a) (cdr b))))))

(define (list-index-ofv v ls)
  (define (ls-help ls index)
    (cond
      ((null? ls) -1)
      ((eqv? (car ls) v) index)
      (else (ls-help (cdr ls) (+ index 1)))))
  (ls-help ls 0))

(define (reverse ls)
  (define (reverse-help ls res)
    (cond
      ((null? ls) '())
      ((null? (cdr ls)) (cons (car ls) res))
      (else (reverse-help (cdr ls) (cons (car ls) res)))))
  (reverse-help ls '()))

(define (append a b)
  (define (append-help rev-a b)
    (if (null? rev-a)
	b
	(append-help (cdr rev-a) (cons (car rev-a) b))))
  (append-help (reverse a) b))
 

(provide (all-defined-out))
