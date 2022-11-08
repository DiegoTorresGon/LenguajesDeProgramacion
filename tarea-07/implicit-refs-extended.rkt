#lang racket/base

//IMPLICIT-REFS

(struct Program () #:transparent)

(struct a-program Program (stmnt) #:transparent)

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

(struct Statement () #:transparent)

(struct assign-st Statement (var exp1) #:transparent)
(struct print-st Statement (exp1) #:transparent)
(struct seq-st Statement (stmntlist) #:transparent)
(struct if-st Statement (exp1 stmnt1 stmnt2) #:transparent)
(struct while-st Statement (exp1 stmnt1) #:transparent)
(struct var-st Statement (idlist stmnt1) #:transparent)

(struct Value () #:transparent)

(struct int-val Value (n) #:transparent)
(struct bool-val Value (b) #:transparent)
(struct proc-val Value (id body env) #:transparent)


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
					 ((null? store1) (error 'setref! "Referencia inválida ref: ~a en ~a" ref global-store))
					 ((zero? ref1) (cons val (cdr global-store)))
					 (else (cons (car store1) (setref-inner (cdr store1)
															(- ref1 1))))))))
			   (setref-inner global-store ref)))))

(define (interp program)
  (define (interp-exp exp1 env)
	(cond
	  ([const-exp? exp1] (int-val (const-exp-n exp1)))
	  ([zero?-exp? exp1] (let ((val (interp-exp (zero?-exp-exp1 exp1) env)))
						   (if (int-val? val) 
							 (bool-val (zero? (int-val-n val)))
							 (error 'interp "exp1 no es un número ~a" val))))
	  ([diff-exp? exp1] (let ((val1 (interp-exp (diff-exp-exp1 exp1) env)) 
							  (val2 (interp-exp (diff-exp-exp2 exp1) env)))
						  (if (and (int-val? val1) (int-val? val2))
							(int-val (- (int-val-n val1) (int-val-n val2)))
							(error 'interp "los argumentos no son numéricos ~a, ~a" val1 val2))))
	  ([if-exp? exp1] (let ((val1 (interp-exp (if-exp-exp1 exp1) env)))
						(if (bool-val? val1)
						  (if (bool-val-b val1) 
							(interp-exp (if-exp-exp2 exp1) env)
							(interp-exp (if-exp-exp3 exp1) env))
						  (error 'interp "el primero argumento no es bool ~a" val1))))
	  ([var-exp? exp1] (let ((reference (lookup-id (var-exp-var exp1) env)))
						(deref reference)))
	  ([let-exp? exp1] (let ((ref (newref (interp-exp (let-exp-exp1 exp1) env)))
							 (new-env (extend-env (let-exp-var exp1) ref env)))
						 (interp-exp (let-exp-body exp1) new-env)))
	  ([letrec-exp? exp1] (let* ((proc-ref (newref (int-val 0))) 
								(new-env (extend-env (letrec-exp-p-name exp1) proc-ref env)))
							(setref! proc-ref (proc-val (letrec-exp-b-var exp1)
													   (letrec-exp-p-body exp1)
													   new-env))
							(interp-exp (letrec-exp-letrec-body exp1) new-env)))
	  ([set-exp? exp1] (let ((var-ref (lookup-id (set-exp-var exp1) env))
							 (new-val (interp-exp (set-exp-exp1 exp1) env)))
						 (setref! var-ref new-val)))
	  ([call-exp? exp1] (let ((val1 (interp-exp (call-exp-exp1 exp1) env))
							  (val2 (interp-exp (call-exp-exp2 exp1) env)))
						  (if (proc-val? val1)
							(let ((proc-body (proc-val-body val1))
								  (proc-env (extend-env (proc-val-id val1)
														(newref val2)
														(proc-val-env val1))))
							  ;(print (int-val-n val2))
							  (interp-exp proc-body proc-env))
							(error 'interp "Primer argumento no es función ~a" val1))))))
  (define (interp-stmnt stmnt env)
	(cond
	  ([assign-st? stmnt] (let ((assign (interp-exp (set-exp (assign-st-var stmnt) (assign-st-exp1 stmnt)) env)))
							(void)))
	  ([print-st? stmnt] (displayln (interp-exp (print-st-exp1 stmnt) env)))
	  ([seq-st? stmnt] (let ((stmnt-list (seq-st-stmntlist stmnt)))
						 (if (empty? stmnt-list)
						   (error 'interp "seq-st vacío")
						   (if (empty? (cdr stmnt-list))
							 (interp-stmnt (car stmnt-list) env)
							 (let ((first-stmnt (car stmnt-list)))
							   (interp-stmnt first-stmnt env)
							   (interp-stmnt (seq-st (cdr stmnt-list)) env))))))
	  ([if-st? stmnt] (let ((val1 (interp-exp (if-st-exp1 stmnt) env)))
						(if (bool-val? val1)
						  (if (bool-val-b val1)
							(interp-stmnt (if-st-stmnt1 stmnt) env)
							(interp-stmnt (if-st-stmnt2 stmnt) env))
						  (error 'interp "first argument in if is not bool ~a" val1))))
	  ([while-st? stmnt] (let ((val1 (interp-exp (while-st-exp1 stmnt) env)))
						   (if (bool-val? val1)
							 (if (bool-val-b val1)
							   (interp-stmnt (while-st-stmnt1 stmnt) env)
							   (void))
							 (error 'interp "first argument in while is not bool ~a" val1))))
	  ([var-st? stmnt] (let ((new-env (extend-env
									   (if (empty? (var-st-idlist stmnt))
										 (error 'interp "var-st con idlist vacía")
										 (first (var-st-idlist stmnt)))
									   (newref 'uninitialized)
									   env)))
						 (if (empty? (cdr (var-st-idlist stmnt)))
						   (interp-stmnt (var-st-stmnt1 stmnt) new-env)
						   (interp-stmnt (var-st (cdr (var-st-idlist stmnt)) 
												 (var-st-stmnt1 stmnt)) 
										 new-env))))
	  (else (error 'interp "estructura indefinida ~a" stmnt))))
  (init-store!)
  (interp-stmnt (a-program-stmnt program) empty-env))



