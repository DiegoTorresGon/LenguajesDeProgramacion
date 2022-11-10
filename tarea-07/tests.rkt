#lang racket/base

;tests for implicit-refs.rkt, call-by-reference.rkt and call-by-need.rkt

(require rackunit rackunit/text-ui)

(require "implicit-refs.rkt"
		 (only-in "call-by-reference.rkt" interp-r)
		 (only-in "call-by-need.rkt" interp-n interp-n-exp))

(define-test-suite pruebas

    (init-store!)
    (test-case "diference-in-call-method"
      (check-equal?
    	(interp-v-exp 
    	(let-exp 'y (const-exp 0)
    	(let-exp 'z (const-exp 0)
    	  (letrec-exp 'p 'x
    			 (set-exp 'x (const-exp 10))
    			 (seq-exp
    			   (list 
    				 (set-exp 'y (const-exp 1))
    				 (set-exp 'z (const-exp 2))
    				 (call-exp 
    				   (var-exp 'p)
    				   (seq-exp
    					 (list (set-exp 'z (diff-exp
    										 (var-exp 'y)
    										 (diff-exp
    										   (const-exp 0)
    										   (var-exp 'z))))
    						   (var-exp 'y))))
    				 (call-exp
    				   (var-exp 'p)
    				   (var-exp 'y))
    				 (diff-exp
    				   (var-exp 'y)
    				   (diff-exp 
    					 (const-exp 0)
    					 (var-exp 'z))))))))
    			 empty-env)
    	(int-val 4)))

	(init-store!)
	(check-equal?
	  (interp-n-exp
		(seq-exp
		  (list
			(const-exp 10)))
		empty-env)
	  (int-val 10))

	(init-store!)
	(check-equal?
	  (interp-n-exp
		(letrec-exp 'f 'x
		  (const-exp 10)
		  (call-exp 
			(var-exp 'f)
			(const-exp 1)))
		empty-env)
	  (int-val 10))

	(init-store!)
	(check-equal?
	  (interp-n-exp
		(letrec-exp 'f 'x
		  (var-exp 'x)
		  (call-exp 
			(var-exp 'f)
			(const-exp 1)))
		empty-env)
	  (int-val 1))

    (test-case "adsf"
      (check-equal?
       (interp-n 
    	 (a-program 
    	   (var-st 
    		  '(y z)
    		  (print-st
    			(letrec-exp 'p 'x
    			  (set-exp 'x (const-exp 10))
				  (const-exp 10))))))
       (void)))
	
	(display "Empiezan los ejemplos diferenciadores.\n")
	(display "implicit-refs (call-by-value): \n")
    (test-case "diference-in-call-method"
      (check-equal? 
       (interp-v 
    	 (a-program 
    	   (var-st 
    		  '(y z)
    		  (print-st
    			(letrec-exp 'p 'x
    			  (set-exp 'x (const-exp 10))
    			  (seq-exp
    			    (list 
    				  (set-exp 'y (const-exp 1))
    				  (set-exp 'z (const-exp 2))
    				  (call-exp 
    				    (var-exp 'p)
    				    (seq-exp
    					  (list (set-exp 'z (diff-exp
    										  (var-exp 'y)
    										  (diff-exp
    										    (const-exp 0)
    										    (var-exp 'z))))
    							(var-exp 'y))))
    				  (call-exp
    				    (var-exp 'p)
    				    (var-exp 'y))
    				  (diff-exp
    				    (var-exp 'y)
    				    (diff-exp 
    					  (const-exp 0)
    					  (var-exp 'z))))))))))
       (void)))

	(display "\ncall-by-reference:\n")
    (test-case "diference-in-call-method"
      (check-equal? 
       (interp-r 
    	 (a-program 
    	   (var-st 
    		  '(y z)
    		  (print-st
    			(letrec-exp 'p 'x
    			  (set-exp 'x (const-exp 10))
    			  (seq-exp
    			    (list 
    				  (set-exp 'y (const-exp 1))
    				  (set-exp 'z (const-exp 2))
    				  (call-exp 
    				    (var-exp 'p)
    				    (seq-exp
    					  (list (set-exp 'z (diff-exp
    										  (var-exp 'y)
    										  (diff-exp
    										    (const-exp 0)
    										    (var-exp 'z))))
    							(var-exp 'y))))
    				  (call-exp
    				    (var-exp 'p)
    				    (var-exp 'y))
    				  (diff-exp
    				    (var-exp 'y)
    				    (diff-exp 
    					  (const-exp 0)
    					  (var-exp 'z))))))))))
       (void)))

	(display "\ncall-by-need:\n")
    (test-case "diference-in-call-method"
      (check-equal? 
       (interp-n 
    	 (a-program 
    	   (var-st 
    		  '(y z)
    		  (print-st
    			(letrec-exp 'p 'x
    			  (set-exp 'x (const-exp 10))
    			  (seq-exp
    			    (list 
    				  (set-exp 'y (const-exp 1))
    				  (set-exp 'z (const-exp 2))
    				  (call-exp 
    				    (var-exp 'p)
    				    (seq-exp
    					  (list (set-exp 'z (diff-exp
    										  (var-exp 'y)
    										  (diff-exp
    										    (const-exp 0)
    										    (var-exp 'z))))
    							(var-exp 'y))))
    				  (call-exp
    				    (var-exp 'p)
    				    (var-exp 'y))
    				  (diff-exp
    				    (var-exp 'y)
    				    (diff-exp 
    					  (const-exp 0)
    					  (var-exp 'z))))))))))
       (void)))

	(display "\n\nTerminan los ejemplos difernciadores.")

    (test-case "test1"
      (check-equal?
    	(interp-v
    	  (a-program
    		(var-st '(x)
    				(print-st (const-exp 1)))))
    	(void)))

	(test-case "recursion-test"
	  (check-equal?
		(interp-v (a-program (var-st (list 'x 'z) 
								   (seq-st (list
											 (assign-st 'x (const-exp 10))
											 (assign-st 'z
														(letrec-exp 'f 'y
															(if-exp
															  (zero?-exp (var-exp 'y))
															  (const-exp 0)
															  (diff-exp 
																(var-exp 'y)
																(diff-exp
																  (const-exp 0)
																  (call-exp 
																	(var-exp 'f)
																	(diff-exp
																	(var-exp 'y)
																	(const-exp 1))))))
															(call-exp
															  (var-exp 'f)
															  (var-exp 'x))))
											 (print-st (var-exp 'z)))))))
		(void))))
	
(run-tests pruebas)

