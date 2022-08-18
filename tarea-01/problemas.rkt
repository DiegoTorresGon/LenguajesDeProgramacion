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
 
(define (repeat ls n)
  (define (repeat-help ls n res)
    (if (= n 0)
	res
	(repeat-help ls (- n 1) (append res ls))))
  (repeat-help ls n '()))

(define (same-lists* a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or (and (null? a) (not (null? b))) (and (null? b) (not (null? a)))) #f)
    ((and (pair? (car a)) (pair? (car b))) 
     (and (same-lists* (car a) (car b)) (same-lists* (cdr a) (cdr b))))
    ((and (not (pair? (car a))) (not (pair? (car b))) (equal? (car a) (car b))) 
     (same-lists* (cdr a) (cdr b)))
    (else #f)))

;Problema 12
;((w x) y (z)) := ((w. (x . ())) . (y . ((z . ()) . ()))) 

(define (binary->natural n_rev)
  (define (binary-help n_rev start_place value)
    (if (empty? n_rev)
      value
      (binary-help (cdr n_rev) 
			  (* 2 start_place) 
			  (+ value (* (car n_rev) start_place)))))
  (binary-help n_rev 1 0))

(define (div a b)
  (define (div-help a b res)
    (cond
      ((= b 0) (error "div entre cero"))
      ((< a 0) (error "algo salió mal"))
      ((= a 0) res)
      (else (div-help (- a b) b (+ res 1)))))
  (cond
    ((and (< a 0) (< b 0)) (div-help (- a) (- b) 0))
    ((< a 0) (- (div-help (- a) b 0)))
    ((< b 0) (- (div-help a (- b) 0)))
    (else (div-help a b 0))))

(define (append-map proc ls)
  (define (append-map-help ls res)
    (if (null? ls)
	res
	(append-map-help (cdr ls) (append res (proc (car ls))))))
  (append-map-help ls '()))
  
(define (set-difference ls exclude)
  (filter (lambda (x) (= -1 (list-index-ofv x exclude))) ls))
  	
(define (foldr bin-proc start ls)
  (define (foldr-help bin-proc start ls-rev)
    (if (null? ls-rev)
      	start
       	(foldr-help bin-proc (bin-proc (car ls-rev) start) (cdr ls-rev))))
  (foldr-help bin-proc start (reverse ls)))

(define (powerset ls)
  (cond 
    ((null? ls) '(()))
    (else 
      (let ((pow-cdr (powerset (cdr ls))))
      (append (map (lambda (x) (cons (car ls) x)) pow-cdr) pow-cdr)))))

(define (cartesian-product ls)
  (define (cartesian-help a b res)
    (if (null? a)
      res
      (cartesian-help (cdr a) b 
		(append res (map (lambda (x) (list (car a) x)) b)))))
  (if (null? ls) 
      '()
      (foldr 
	(lambda (x res) (cartesian-help res x '()))
	(car ls)
	(cdr ls))))
 
;foldr procedure

(define (insertL-fr x y ls)
  (foldr (lambda (a res) (if (eqv? a x)
			     (cons y (cons x res))
			     (cons a res)))
	 '() 
	 ls))

(define (filter-fr pred ls)
  (foldr (lambda (a res) (if (pred a)
			     (cons a res)
			     res))
	 '()
	 ls))

(define (map-fr proc ls)
  (foldr (lambda (a res) (cons (proc a) res)) '() ls))

(define (append-fr a b)
  (foldr (lambda (x res) (cons x res)) b a)) 

(define (reverse-fr ls)
  (foldr (lambda (x res) (append res (list x))) '() ls))

(define (binary->natural-fr bin_rev)
  (foldr (lambda (x res) (+ x (* 2 res)))
	 0 
	 bin_rev))

(define (set-difference-fr ls exclude)
  (filter-fr (lambda (x) (= -1 (list-index-ofv x exclude))) ls))

(define (powerset-fr ls)
  (foldr (lambda (x res) 
	   (append-fr (map-fr (lambda (z) (cons x z)) res) res))
	 '(())
	 ls))

(define snowball 
  (letrec
    ((odd-case
       (lambda (fix-odd)
	 (lambda (x)
	   (cond
	     ((and (exact-integer? x) (positive? x) (odd? x))
	      (snowball (add1 (* x 3))))
	     (else (fix-odd x))))))
     (even-case
       (lambda (fix-even)
	 (lambda (x)
	   (cond
	     ((and (exact-integer? x) (positive? x) (even? x))
	      (snowball (/ x 2)))
	     (else (fix-even x))))))
     (one-case
       (lambda (fix-one)
	 (lambda (x)
	   (cond
	     ((zero? (sub1 x)) 1)
	     (else (fix-one x))))))
     (base
       (lambda (x)
	 (error 'error "Invalid value ~s~n" x))))
	(one-case (odd-case (even-case base)))))
  

(provide (all-defined-out))
