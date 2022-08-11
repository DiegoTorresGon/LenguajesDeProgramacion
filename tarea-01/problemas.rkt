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
	
(provide (all-defined-out))
