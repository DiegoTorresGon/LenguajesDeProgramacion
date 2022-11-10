#lang racket

;;CALL-BY-REFERENCE

(require "implicit-refs.rkt")

(define (interp-r-const-exp exp1 env)
	(int-val (const-exp-n exp1)))

(define (interp-r-zero?-exp exp1 env)
	(let ((val (interp-r-exp (zero?-exp-exp1 exp1) env)))
	  (if (int-val? val) 
		(bool-val (zero? (int-val-n val)))
		(error 'interp-r "exp1 no es un número ~a" val))))

(define (interp-r-diff-exp exp1 env)
	(let ((val1 (interp-r-exp (diff-exp-exp1 exp1) env)) 
		  (val2 (interp-r-exp (diff-exp-exp2 exp1) env)))
	  (if (and (int-val? val1) (int-val? val2))
		(int-val (- (int-val-n val1) (int-val-n val2)))
		(error 'interp-r "los argumentos no son numéricos ~a, ~a" val1 val2))))

(define (interp-r-if-exp exp1 env)
  (let ((val1 (interp-r-exp (if-exp-exp1 exp1) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1) 
		(interp-r-exp (if-exp-exp2 exp1) env)
		(interp-r-exp (if-exp-exp3 exp1) env))
	  (error 'interp-r "el primero argumento no es bool ~a" val1))))

(define (interp-r-var-exp exp1 env)
  (let ((reference (lookup-id (var-exp-var exp1) env)))
	(deref reference)))

(define (interp-r-let-exp exp1 env)
  (let* ((ref (newref (interp-r-exp (let-exp-exp1 exp1) env)))
		(new-env (extend-env (let-exp-var exp1) ref env)))
	(interp-r-exp (let-exp-body exp1) new-env)))

(define (interp-r-letrec-exp exp1 env)
  (let* ((proc-ref (newref (int-val 0))) 
		 (new-env (extend-env (letrec-exp-p-name exp1) proc-ref env)))
	(setref! proc-ref (proc-val (letrec-exp-b-var exp1)
								(letrec-exp-p-body exp1)
								new-env))
	(interp-r-exp (letrec-exp-letrec-body exp1) new-env)))

(define (interp-r-set-exp exp1 env)
  (let ((var-ref (lookup-id (set-exp-var exp1) env))
		(new-val (interp-r-exp (set-exp-exp1 exp1) env)))
	(setref! var-ref new-val)))

(define (interp-r-call-exp exp1 env)
  (let ((val1 (interp-r-exp (call-exp-exp1 exp1) env))
		(val2 (interp-r-exp (call-exp-exp2 exp1) env)))
	(if (proc-val? val1)
	  (let ((proc-body (proc-val-body val1))
			(proc-env (extend-env (proc-val-id val1)
								  (if (var-exp? (call-exp-exp2 exp1))
									(lookup-id (var-exp-var (call-exp-exp2 exp1)) env)
									(newref val2))
								  (proc-val-env val1))))
		(interp-r-exp proc-body proc-env))
	  (error 'interp-r "Primer argumento no es función ~a" val1))))

(define (interp-r-proc-exp exp1 env)
  (proc-val (proc-exp-b-var exp1)
			(proc-exp-p-body exp1)
			env))

(define (interp-r-seq-exp exp1 env)
  (let ((explist (seq-exp-explist exp1)))
	(if (empty? explist)
	  (error 'interp-r "seq-exp vacío")
  	  (if (empty? (cdr explist))
		(interp-r-exp (car explist) env)
		(let ((first-exp (car explist)))
		  (interp-r-exp first-exp env)
  		  (interp-r-exp (seq-exp (cdr explist)) env))))))

(define (interp-r-exp exp1 env)
  (cond
    ([const-exp? exp1] (interp-r-const-exp exp1 env))
    ([zero?-exp? exp1] (interp-r-zero?-exp exp1 env))
    ([diff-exp? exp1] (interp-r-diff-exp exp1 env))
    ([if-exp? exp1] (interp-r-if-exp exp1 env))
    ([var-exp? exp1] (interp-r-var-exp exp1 env))
    ([let-exp? exp1] (interp-r-let-exp exp1 env))
    ([letrec-exp? exp1] (interp-r-letrec-exp exp1 env))
    ([set-exp? exp1] (interp-r-set-exp exp1 env))
    ([call-exp? exp1] (interp-r-call-exp exp1 env))
	([proc-exp? exp1] (interp-r-proc-exp exp1 env))
	([seq-exp? exp1] (interp-r-seq-exp exp1 env))
	(else (error 'interp-r "Estructura indefinida ~a" exp1))))

(define (interp-r-assign-st stmnt env)
  (let ((assign (interp-r-exp (set-exp (assign-st-var stmnt) (assign-st-exp1 stmnt)) env)))
	(void)))

(define (interp-r-print-st stmnt env)
  (displayln (interp-r-exp (print-st-exp1 stmnt) env)))

(define (interp-r-seq-st stmnt env)
  (let ((stmnt-list (seq-st-stmntlist stmnt)))
	(if (empty? stmnt-list)
	  (error 'interp-r "seq-st vacío")
  	  (if (empty? (cdr stmnt-list))
		(interp-r-stmnt (car stmnt-list) env)
		(let ((first-stmnt (car stmnt-list)))
		  (interp-r-stmnt first-stmnt env)
  		  (interp-r-stmnt (seq-st (cdr stmnt-list)) env))))))

(define (interp-r-if-st stmnt env)
  (let ((val1 (interp-r-exp (if-st-exp1 stmnt) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1)
		(interp-r-stmnt (if-st-stmnt1 stmnt) env)
		(interp-r-stmnt (if-st-stmnt2 stmnt) env))
	  (error 'interp-r "first argument in if is not bool ~a" val1))))

(define (interp-r-while-st stmnt env)
  (let ((val1 (interp-r-exp (while-st-exp1 stmnt) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1)
		(interp-r-stmnt (while-st-stmnt1 stmnt) env)
		(void))
	  (error 'interp-r "first argument in while is not bool ~a" val1))))

(define (interp-r-var-st stmnt env)
  (let ((new-env (extend-env
				   (if (empty? (var-st-idlist stmnt))
					 (error 'interp-r "var-st con idlist vacía")
					 (first (var-st-idlist stmnt)))
				   (newref 'uninitialized)
				   env)))
	(if (empty? (cdr (var-st-idlist stmnt)))
	  (interp-r-stmnt (var-st-stmnt1 stmnt) new-env)
	  (interp-r-stmnt (var-st (cdr (var-st-idlist stmnt)) 
							(var-st-stmnt1 stmnt)) 
					new-env))))

(define (interp-r-stmnt stmnt env)
  (cond
    ([assign-st? stmnt] (interp-r-assign-st stmnt env))
    ([print-st? stmnt] (interp-r-print-st stmnt env))
    ([seq-st? stmnt] (interp-r-seq-st stmnt env))
    ([if-st? stmnt] (interp-r-if-st stmnt env))
    ([while-st? stmnt] (interp-r-while-st stmnt env))
    ([var-st? stmnt] (interp-r-var-st stmnt env))
    (else (error 'interp-r "estructura indefinida ~a" stmnt))))

(define (interp-r program)
  (init-store!)
  (interp-r-stmnt (a-program-stmnt program) empty-env))

(provide (all-defined-out))
;; Simple test of the extension of the language.

;;;;;;;;(define recursion-test 
;;;;;;;;  (interp-r (a-program 
;;;;;;;;			(var-st '(x z) 
;;;;;;;;					(seq-st (list (assign-st 'x (const-exp 10))
;;;;;;;;								  (assign-st 'z
;;;;;;;;											 (letrec-exp 
;;;;;;;;											   'f 'y
;;;;;;;;												(if-exp
;;;;;;;;												  (zero?-exp (var-exp 'y))
;;;;;;;;												  (const-exp 0)
;;;;;;;;												  (diff-exp 
;;;;;;;;													(var-exp 'y)
;;;;;;;;													(diff-exp
;;;;;;;;													  (const-exp 0)
;;;;;;;;													  (call-exp 
;;;;;;;;														(var-exp 'f)
;;;;;;;;														(diff-exp
;;;;;;;;														  (var-exp 'y)
;;;;;;;;														  (const-exp 1))))))
;;;;;;;;												(call-exp
;;;;;;;;													  (var-exp 'f)
;;;;;;;;													  (var-exp 'x))))
;;;;;;;;								  (print-st (var-exp 'z))
;;;;;;;;								  (print-st (var-exp 'x))))))))

;;;;;;;;(define call-by-reference-test
;;;;;;;;  (interp-r (a-program
;;;;;;;;			(var-st '(x)
;;;;;;;;					(seq-st (list (assign-st 'x (const-exp 10))
;;;;;;;;								  (print-st (letrec-exp 'f 'y
;;;;;;;;											  (set-exp 'y (const-exp 1))
;;;;;;;;											  (call-exp (var-exp 'f)
;;;;;;;;														(var-exp 'x))))
;;;;;;;;								  (print-st (var-exp 'x))))))))


