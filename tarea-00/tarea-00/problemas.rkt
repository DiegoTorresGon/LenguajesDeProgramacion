#lang racket

;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (* pi (expt r 2)))

;; 3.
(define (circle-properties r)
  (list (area-circle r) (* 2 pi r)))

;; 4.
(define (rectangle-properties rec)
  (let ((a (car rec)) (b (cadr rec)))
  (list (* a b) (+ (* 2 a) (* 2 b)))))

;; 5.
(define (find-needle ls)
  (define (find-help ls ind)
    (cond 
      ((empty? ls) -1)
      ((equal? (car ls) "needle") (+ ind 1))
      (else (find-help (rest ls) (+ ind 1)))))
  (find-help ls -1))

(if (condicion) valor_verdadero valor_negativo)
;; 6.
(define (abs x)
  (if (< x 0) (- x) x))

;; 7.
(define (inclis1 ls)
  (map (lambda (x) (+ x 1)) ls))

;; 8.
(define (even? x)
  (define (even-h x neg)
  	(cond 
	  ((= x 0) (not neg))
	  (else (even-h (- x 1) (not neg)))))
  (even-h x #f))

;; 9.
(define another-add
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (add1 (another-add (sub1 n) m))))))

(provide (all-defined-out))