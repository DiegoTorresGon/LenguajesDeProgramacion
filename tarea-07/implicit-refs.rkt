#lang racket

;;IMPLICIT-REFS

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
(struct seq-exp Expression (explist) #:transparent)
(struct proc-exp Expression (b-var p-body) #:transparent)

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


(struct binding (id value) #:transparent) ; Environment is a listof binding

(define empty-env '())

(define (extend-env id value env)  
  (cons (binding id value) env)) 

(define (lookup-id id env)
  (cond 
	([empty? env] (error 'lookup-id "identificador no está enlazado: ~a" id))
	([eq? id (binding-id (first env))] (binding-value (first env)))
	(else (lookup-id id (rest env)))))

; HAY QUE DEFINIR EL STORE.

(define global-store 'uninitialized)

(define empty-store (lambda () empty))

(define get-store (lambda () global-store))

(define init-store! (lambda () (set! global-store (empty-store))))

(define newref (lambda (val)
				 (let ((next-ref (length global-store)))
				   (set! global-store (append global-store (list val)))
				   next-ref)))

(define reference? integer?)

(define deref
  (lambda (ref)
	(list-ref global-store ref)))


(define setref!
  (lambda (ref val)
	(set! global-store
	  (if (integer? ref)
		(letrec ((setref-inner
				 (lambda (store1 ref1)
				   (cond
					 ((null? store1) (error 'setref! "Referencia inválida ref: ~a en ~a" ref global-store))
					 ((zero? ref1) (cons val (cdr store1)))
					 (else (cons (car store1) (setref-inner (cdr store1)
															(- ref1 1))))))))
			   (setref-inner global-store ref))
		(error 'setref! "ref no está bien ~a" ref)))))

(define (interp-v-const-exp exp1 env)
	(int-val (const-exp-n exp1)))

(define (interp-v-zero?-exp exp1 env)
	(let ((val (interp-v-exp (zero?-exp-exp1 exp1) env)))
	  (if (int-val? val) 
		(bool-val (zero? (int-val-n val)))
		(error 'interp-v "exp1 no es un número ~a" val))))

(define (interp-v-diff-exp exp1 env)
	(let ((val1 (interp-v-exp (diff-exp-exp1 exp1) env)) 
		  (val2 (interp-v-exp (diff-exp-exp2 exp1) env)))
	  (if (and (int-val? val1) (int-val? val2))
		(int-val (- (int-val-n val1) (int-val-n val2)))
		(error 'interp-v "los argumentos no son numéricos ~a, ~a" val1 val2))))

(define (interp-v-if-exp exp1 env)
  (let ((val1 (interp-v-exp (if-exp-exp1 exp1) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1) 
		(interp-v-exp (if-exp-exp2 exp1) env)
		(interp-v-exp (if-exp-exp3 exp1) env))
	  (error 'interp-v "el primero argumento no es bool ~a" val1))))

(define (interp-v-var-exp exp1 env)
  (let ((reference (lookup-id (var-exp-var exp1) env)))
	(deref reference)))

(define (interp-v-let-exp exp1 env)
  (let* ((ref (newref (interp-v-exp (let-exp-exp1 exp1) env)))
		(new-env (extend-env (let-exp-var exp1) ref env)))
	(interp-v-exp (let-exp-body exp1) new-env)))

(define (interp-v-letrec-exp exp1 env)
  (let* ((proc-ref (newref (int-val 1))) 
		 (new-env (extend-env (letrec-exp-p-name exp1) proc-ref env)))
	(setref! proc-ref (proc-val (letrec-exp-b-var exp1)
								(letrec-exp-p-body exp1)
								new-env))
	;(displayln new-env)
	;(displayln (get-store))
	;(displayln (deref (lookup-id (letrec-exp-p-name exp1) new-env)))
	(interp-v-exp (letrec-exp-letrec-body exp1) new-env)))

(define (interp-v-set-exp exp1 env)
  (let ((var-ref (lookup-id (set-exp-var exp1) env))
		(new-val (interp-v-exp (set-exp-exp1 exp1) env)))
	;(printf "Store en set-exp: ~a\n" (get-store))
	(setref! var-ref new-val)))
	;(printf "Store despues de set-exp: ~a\n" (get-store))))

(define (interp-v-call-exp exp1 env)
  (let ((val1 (interp-v-exp (call-exp-exp1 exp1) env))
		(val2 (interp-v-exp (call-exp-exp2 exp1) env)))
	(if (proc-val? val1)
	  (let ((proc-body (proc-val-body val1))
			(proc-env (extend-env (proc-val-id val1)
								  (newref val2)
								  (proc-val-env val1))))
		(interp-v-exp proc-body proc-env))
	  (error 'interp-v "Primer argumento no es función ~a \n ~a \n\n ~a \n ambiente: ~a \n store: ~a" 
			 (call-exp-exp1 exp1)
			 val1
			 exp1
			 env
			 (get-store)))))

(define (interp-v-proc-exp exp1 env)
  (proc-val (proc-exp-b-var exp1)
			(proc-exp-p-body exp1)
			env))

(define (interp-v-seq-exp exp1 env)
  (let ((explist (seq-exp-explist exp1)))
	(if (empty? explist)
	  (error 'interp-v "seq-exp vacío")
  	  (if (empty? (cdr explist))
		(interp-v-exp (car explist) env)
		(let ((first-exp (car explist)))
		  (interp-v-exp first-exp env)
  		  (interp-v-exp (seq-exp (cdr explist)) env))))))

(define (interp-v-exp exp1 env)
  (cond
    ([const-exp? exp1] (interp-v-const-exp exp1 env))
    ([zero?-exp? exp1] (interp-v-zero?-exp exp1 env))
    ([diff-exp? exp1] (interp-v-diff-exp exp1 env))
    ([if-exp? exp1] (interp-v-if-exp exp1 env))
    ([var-exp? exp1] (interp-v-var-exp exp1 env))
    ([let-exp? exp1] (interp-v-let-exp exp1 env))
    ([letrec-exp? exp1] (interp-v-letrec-exp exp1 env))
    ([set-exp? exp1] (interp-v-set-exp exp1 env))
    ([call-exp? exp1] (interp-v-call-exp exp1 env))
	([proc-exp? exp1] (interp-v-proc-exp exp1 env))
	([seq-exp? exp1] (interp-v-seq-exp exp1 env))
	(else (error 'interp-v-exp "Estructura indefinida ~a" exp1))))

(define (interp-v-assign-st stmnt env)
  (let ((assign (interp-v-exp (set-exp (assign-st-var stmnt) (assign-st-exp1 stmnt)) env)))
	(void)))

(define (interp-v-print-st stmnt env)
  (displayln (interp-v-exp (print-st-exp1 stmnt) env)))

(define (interp-v-seq-st stmnt env)
  (let ((stmnt-list (seq-st-stmntlist stmnt)))
	(if (empty? stmnt-list)
	  (error 'interp-v "seq-st vacío")
  	  (if (empty? (cdr stmnt-list))
		(interp-v-stmnt (car stmnt-list) env)
		(let ((first-stmnt (car stmnt-list)))
		  (interp-v-stmnt first-stmnt env)
  		  (interp-v-stmnt (seq-st (cdr stmnt-list)) env))))))

(define (interp-v-if-st stmnt env)
  (let ((val1 (interp-v-exp (if-st-exp1 stmnt) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1)
		(interp-v-stmnt (if-st-stmnt1 stmnt) env)
		(interp-v-stmnt (if-st-stmnt2 stmnt) env))
	  (error 'interp-v "first argument in if is not bool ~a" val1))))

(define (interp-v-while-st stmnt env)
  (let ((val1 (interp-v-exp (while-st-exp1 stmnt) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1)
		(interp-v-stmnt (while-st-stmnt1 stmnt) env)
		(void))
	  (error 'interp-v "first argument in while is not bool ~a" val1))))

(define (interp-v-var-st stmnt env)
  (let ((new-env (extend-env
				   (if (empty? (var-st-idlist stmnt))
					 (error 'interp-v "var-st con idlist vacía")
					 (first (var-st-idlist stmnt)))
				   (newref 'uninitialized)
				   env)))
	(if (empty? (cdr (var-st-idlist stmnt)))
	  (interp-v-stmnt (var-st-stmnt1 stmnt) new-env)
	  (interp-v-stmnt (var-st (cdr (var-st-idlist stmnt)) 
							(var-st-stmnt1 stmnt)) 
					new-env))))

(define (interp-v-stmnt stmnt env)
  (cond
    ([assign-st? stmnt] (interp-v-assign-st stmnt env))
    ([print-st? stmnt] (interp-v-print-st stmnt env))
    ([seq-st? stmnt] (interp-v-seq-st stmnt env))
    ([if-st? stmnt] (interp-v-if-st stmnt env))
    ([while-st? stmnt] (interp-v-while-st stmnt env))
    ([var-st? stmnt] (interp-v-var-st stmnt env))
    (else (error 'interp-v-stmnt "estructura indefinida ~a" stmnt))))

(define (interp-v program)
  (init-store!)
  (interp-v-stmnt (a-program-stmnt program) empty-env))

(provide (all-defined-out))

