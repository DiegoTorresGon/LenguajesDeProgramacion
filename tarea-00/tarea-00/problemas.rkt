#lang racket

;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (* pi (expt r 2)))

;; 3.
(define (circle-properties r)
  (list (area-circle r) (* 2 pi r))

;; 4.
(define (rectangle-properties rec)
  (let ((a (car rec)) (b (cdr rec))))
  (list (* a b) (+ (* 2 a) (* 2 b))))

;; 5.
(define (find-needle ls)
  (define (find-help ls ind)
    (cond 
      ((null? ls) -1)
      ((= (car ls) "needle") (+ ind 1))
      (else (find-help ls (+ ind 1)))))
  (find-help ls -1))

;; 6.
(define (abs x)
  ...)

;; 7.
(define (inclis1 ls)
  ...)

;; 8.
(define (even? x)
  ...)

;; 9.
(define another-add
  (lambda (n m)
    ...))

(provide (all-defined-out))
