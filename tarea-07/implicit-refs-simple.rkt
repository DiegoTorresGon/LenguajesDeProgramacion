#lang racket/base

//IMPLICIT-REFS

(struct Program () #:transparent)

(struct a-program Program (exp1) #:transparent)

(struct Expression () #:transparent)

(struct const-exp Expression (n) #:transparent)
(struct diff-exp Expression (exp1 exp2) #:transparent)
(struct zero?-exp Expression (exp1) #:transparent)
(struct if-exp Expression (exp1 exp2 exp3) #:transparent)
(struct var-exp Expression (var) #:transparent)
(struct let-exp Expression (var exp1 body) #:transparent)
(struct letrec-exp Expression (p-name b-var p-body letrec-body) #:transparent)
(struct set-exp Expression (var exp1) #:transparent)
(struct call-exp Expression (exp1 exp2) #:transparent)

(struct Value () #:transparent)

(struct int-val Value (n) #:transparent)
(struct bool-val Value (b) #:transparent)
(struct proc-val Value (id body env))


(struct binding (id value)) ; Environment is a listof binding

(define empty-env empty)

(define (extend-env id value env)  
  (cons (binding id value) env)) 

(define (lookup-id id env)
  (cond 
	([empty? env] (error 'lookup-id "identificador no está enlazado: ~a" id))
	([eq? id (binding-id (first env))] (binding-value (first env)))
	(else (lookup-id id (rest env)))))

; HAY QUE DEFINIR EL STORE.

(define global-store 'uninitialized)

(define empty-store (lambda () '()))

(define get-store (lambda () global-store))

(define init-store! (lambda () (set! global-store (empty-store))))

(define newref (lambda (val)
				 (let ((next-ref (length global-store)))
				   (set! global-store (append global-store (list val)))
				   next-ref)))

(define deref
  (lambda (ref)
	(list-ref global-store ref)))


(define setref!
  (lambda (ref val)
	(set! global-store
	  (letrec ((setref-inner
				 (lambda (store1 ref1)
				   (cond
					 ((null? store1) (report-invalid-reference ref gloal-store))
					 ((zero? ref1) (cons val (cdr global-store)))
					 (else (cons (car store1) (setref-inner (cdr store1)
															(- ref1 1))))))))
			   (setref-inner global-store ref)))))

(define (interp-const-exp exp1 env)
	(int-val (const-exp-n exp1)))

(define (interp-zero?-exp exp1 env)
	(let ((val (interp-exp (zero?-exp-exp1 exp1) env)))
	  (if (int-val? val) 
		(bool-val (zero? (int-val-n val)))
		(error 'interp "exp1 no es un número ~a" val))))

(define (interp-diff-exp exp1 env)
	(let ((val1 (interp-exp (diff-exp-exp1 exp1) env)) 
		  (val2 (interp-exp (diff-exp-exp2 exp1) env)))
	  (if (and (int-val? val1) (int-val? val2))
		(int-val (- (int-val-n val1) (int-val-n val2)))
		(error 'interp "los argumentos no son numéricos ~a, ~a" val1 val2))))

(define (interp-if-exp exp1 env)
  (let ((val1 (interp-exp (if-exp-exp1 exp1) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1) 
		(interp-exp (if-exp-exp2 exp1) env)
		(interp-exp (if-exp-exp3 exp1) env))
	  (error 'interp "el primero argumento no es bool ~a" val1))))

(define (interp-var-exp exp1 env)
  (let ((reference (lookup-id (var-exp-var exp1) env)))
	(deref reference)))

(define (interp-let-exp exp1 env)
  (let* ((ref (newref (interp-exp (let-exp-exp1 exp1) env)))
		(new-env (extend-env (let-exp-var exp1) ref env)))
	(interp-exp (let-exp-body exp1) new-env)))

(define (interp-letrec-exp exp1 env)
  (let* ((proc-ref (newref (int-val 0))) 
		 (new-env (extend-env (letrec-exp-p-name exp1) proc-ref env)))
	(setref! proc-ref (proc-val (letrec-exp-b-var exp1)
								(letrec-exp-p-body exp1)
								new-env))
	(interp-exp (letrec-exp-letrec-body exp1) new-env)))

(define (interp-set-exp exp1 env)
  (let ((var-ref (lookup-id (set-exp-var exp1) env))
		(new-val (interp-exp (set-exp-exp1 exp1) env)))
	(setref! var-ref new-val)))

(define (interp-call-exp exp1 env)
  (let ((val1 (interp-exp (call-exp-exp1 exp1) env))
		(val2 (interp-exp (call-exp-exp2 exp1) env)))
	(if (proc-val? val1)
	  (let ((proc-body (proc-val-body val1))
			(proc-env (extend-env (proc-val-id val1)
								  (newref val2)
								  (proc-val-env val1))))
		(interp-exp proc-body proc-env))
	  (error 'interp "Primer argumento no es función ~a" val1))))

(define (interp-exp exp1 env)
  (cond
    ([const-exp? exp1] (interp-const-exp exp1 env))
    ([zero?-exp? exp1] (interp-zero?-exp exp1 env))
    ([diff-exp? exp1] (interp-diff-exp exp1 env))
    ([if-exp? exp1] (interp-if-exp exp1 env))
    ([var-exp? exp1] (interp-var-exp exp1 env))
    ([let-exp? exp1] (interp-let-exp exp1 env))
    ([letrec-exp? exp1] (interp-letrec-exp exp1 env))
    ([set-exp? exp1] (interp-set-exp exp1 env))
    ([call-exp? exp1] (interp-call-exp exp1 env))))

(define (interp program)
  (init-store!)
  (interp-exp (a-program-exp1 program) empty-env))


;;Simple recursive function test.

(interp (a-program (letrec-exp 'f 'x (if-exp (zero?-exp (var-exp 'x))
											 (const-exp 0)
											 (diff-exp (call-exp (var-exp 'f)
																 (diff-exp (var-exp 'x)
																		   (const-exp 1)))
													   (diff-exp (const-exp 0)
																 (var-exp 'x))))
							   (call-exp (var-exp 'f) (const-exp 10)))))


