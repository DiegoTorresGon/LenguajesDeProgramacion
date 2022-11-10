#lang racket

;;CALL-BY-NEED
(require "implicit-refs.rkt")

(struct thunk (exp1 env) #:transparent)

(define (interp-n-const-exp exp1 env)
	(int-val (const-exp-n exp1)))

(define (interp-n-zero?-exp exp1 env)
	(let ((val (interp-n-exp (zero?-exp-exp1 exp1) env)))
	  (if (int-val? val) 
		(bool-val (zero? (int-val-n val)))
		(error 'interp-n "exp1 no es un número ~a" val))))

(define (interp-n-diff-exp exp1 env)
	(let ((val1 (interp-n-exp (diff-exp-exp1 exp1) env)) 
		  (val2 (interp-n-exp (diff-exp-exp2 exp1) env)))
	  (if (and (int-val? val1) (int-val? val2))
		(int-val (- (int-val-n val1) (int-val-n val2)))
		(error 'interp-n "los argumentos no son numéricos ~a, ~a" val1 val2))))

(define (interp-n-if-exp exp1 env)
  (let ((val1 (interp-n-exp (if-exp-exp1 exp1) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1) 
		(interp-n-exp (if-exp-exp2 exp1) env)
		(interp-n-exp (if-exp-exp3 exp1) env))
	  (error 'interp-n "el primero argumento no es bool ~a" val1))))

(define (interp-n-var-exp exp1 env)
  (let ((saved-val (lookup-id (var-exp-var exp1) env)))
	(if (reference? saved-val)
	  (let ((deref-val (deref saved-val)))
		(cond 
		  ([Value? deref-val] deref-val)
		  ([thunk? deref-val] (interp-n-exp (thunk-exp1 deref-val) (thunk-env deref-val)))
		  (else (error 'interp-n-var-exp "Valor raro: ~a" deref-val))))
	  (error 'interp-n-var-exp "Valor no es referencia ni thunk ~a" saved-val))))

(define (interp-n-let-exp exp1 env)
  (let* ((ref (newref (interp-n-exp (let-exp-exp1 exp1) env)))
		(new-env (extend-env (let-exp-var exp1) ref env)))
	(interp-n-exp (let-exp-body exp1) new-env)))

(define (interp-n-letrec-exp exp1 env)
  (let* ((proc-ref (newref (int-val 0))) 
		 (new-env (extend-env (letrec-exp-p-name exp1) proc-ref env)))
	(setref! proc-ref (proc-val (letrec-exp-b-var exp1)
								(letrec-exp-p-body exp1)
								new-env))
	(interp-n-exp (letrec-exp-letrec-body exp1) new-env)))

(define (interp-n-set-exp exp1 env)
  (let ((var-ref (lookup-id (set-exp-var exp1) env))
		(new-val (interp-n-exp (set-exp-exp1 exp1) env)))
	(setref! var-ref new-val)))

(define (interp-n-call-exp exp1 env)
  (let ((val1 (interp-n-exp (call-exp-exp1 exp1) env)))
	(if (proc-val? val1)
	  (let ((proc-body (proc-val-body val1))
			(proc-env (extend-env (proc-val-id val1)
								  (if (var-exp? (call-exp-exp2 exp1))
									(lookup-id (var-exp-var (call-exp-exp2 exp1)) env)
									(newref (thunk (call-exp-exp2 exp1) env)))
								  (proc-val-env val1))))
		(interp-n-exp proc-body proc-env))
	  (error 'interp-n "Primer argumento no es función ~a" val1))))

(define (interp-n-proc-exp exp1 env)
  (proc-val (proc-exp-b-var exp1)
			(proc-exp-p-body exp1)
			env))

(define (interp-n-seq-exp exp1 env)
  (let ((explist (seq-exp-explist exp1)))
	(if (empty? explist)
	  (error 'interp-n "seq-exp vacío")
  	  (if (empty? (cdr explist))
		(interp-n-exp (car explist) env)
		(let ((first-exp (car explist)))
		  (interp-n-exp first-exp env)
  		  (interp-n-exp (seq-exp (cdr explist)) env))))))

(define (interp-n-exp exp1 env)
  (cond
    ([const-exp? exp1] (interp-n-const-exp exp1 env))
    ([zero?-exp? exp1] (interp-n-zero?-exp exp1 env))
    ([diff-exp? exp1] (interp-n-diff-exp exp1 env))
    ([if-exp? exp1] (interp-n-if-exp exp1 env))
    ([var-exp? exp1] (interp-n-var-exp exp1 env))
    ([let-exp? exp1] (interp-n-let-exp exp1 env))
    ([letrec-exp? exp1] (interp-n-letrec-exp exp1 env))
    ([set-exp? exp1] (interp-n-set-exp exp1 env))
    ([call-exp? exp1] (interp-n-call-exp exp1 env))
	([proc-exp? exp1] (interp-n-proc-exp exp1 env))
	([seq-exp? exp1] (interp-n-seq-exp exp1 env))
	(else (error 'interp-n "Estructura indefinida ~a" exp1))))

(define (interp-n-assign-st stmnt env)
  (let ((assign (interp-n-exp (set-exp (assign-st-var stmnt) (assign-st-exp1 stmnt)) env)))
	(void)))

(define (interp-n-print-st stmnt env)
  (displayln (interp-n-exp (print-st-exp1 stmnt) env)))

(define (interp-n-seq-st stmnt env)
  (let ((stmnt-list (seq-st-stmntlist stmnt)))
	(if (empty? stmnt-list)
	  (error 'interp-n "seq-st vacío")
  	  (if (empty? (cdr stmnt-list))
		(interp-n-stmnt (car stmnt-list) env)
		(let ((first-stmnt (car stmnt-list)))
		  (interp-n-stmnt first-stmnt env)
  		  (interp-n-stmnt (seq-st (cdr stmnt-list)) env))))))

(define (interp-n-if-st stmnt env)
  (let ((val1 (interp-n-exp (if-st-exp1 stmnt) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1)
		(interp-n-stmnt (if-st-stmnt1 stmnt) env)
		(interp-n-stmnt (if-st-stmnt2 stmnt) env))
	  (error 'interp-n "first argument in if is not bool ~a" val1))))

(define (interp-n-while-st stmnt env)
  (let ((val1 (interp-n-exp (while-st-exp1 stmnt) env)))
	(if (bool-val? val1)
	  (if (bool-val-b val1)
		(interp-n-stmnt (while-st-stmnt1 stmnt) env)
		(void))
	  (error 'interp-n "first argument in while is not bool ~a" val1))))

(define (interp-n-var-st stmnt env)
  (let ((new-env (extend-env
				   (if (empty? (var-st-idlist stmnt))
					 (error 'interp-n "var-st con idlist vacía")
					 (first (var-st-idlist stmnt)))
				   (newref 'uninitialized)
				   env)))
	(if (empty? (cdr (var-st-idlist stmnt)))
	  (interp-n-stmnt (var-st-stmnt1 stmnt) new-env)
	  (interp-n-stmnt (var-st (cdr (var-st-idlist stmnt)) 
							(var-st-stmnt1 stmnt)) 
					new-env))))

(define (interp-n-stmnt stmnt env)
  (cond
    ([assign-st? stmnt] (interp-n-assign-st stmnt env))
    ([print-st? stmnt] (interp-n-print-st stmnt env))
    ([seq-st? stmnt] (interp-n-seq-st stmnt env))
    ([if-st? stmnt] (interp-n-if-st stmnt env))
    ([while-st? stmnt] (interp-n-while-st stmnt env))
    ([var-st? stmnt] (interp-n-var-st stmnt env))
    (else (error 'interp-n "estructura indefinida ~a" stmnt))))

(define (interp-n program)
  (init-store!)
  (interp-n-stmnt (a-program-stmnt program) empty-env))

(provide (all-defined-out))

;; Simple test of the extension of the language.

;;;;;;;;(define recursion-test 
;;;;;;;;  (interp-n (a-program 
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
;;;;;;;;  (interp-n (a-program
;;;;;;;;			(var-st '(x)
;;;;;;;;					(seq-st (list (assign-st 'x (const-exp 10))
;;;;;;;;								  (print-st (letrec-exp 'f 'y
;;;;;;;;											  (set-exp 'y (const-exp 1))
;;;;;;;;											  (call-exp (var-exp 'f)
;;;;;;;;														(var-exp 'x))))
;;;;;;;;								  (print-st (var-exp 'x))))))))

;;;;;;;;(define call-by-need-test
;;;;;;;;  (interp-n (a-program
;;;;;;;;			(print-st (letrec-exp 'f 'x (const-exp 10)
;;;;;;;;								  (call-exp (var-exp 'f) 
;;;;;;;;											(const-exp "asdf")))))))

;;;;;;;;(define call-by-need-test-2
;;;;;;;;  (interp-n (a-program
;;;;;;;;			(print-st (letrec-exp 'f 'x (var-exp 'x)
;;;;;;;;								  (call-exp (var-exp 'f)
;;;;;;;;											(let-exp 'x (const-exp 10)
;;;;;;;;													 (var-exp 'x))))))))


