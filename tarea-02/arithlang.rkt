#lang plait

(define-type ArithC
	(NumberC [num : Number])
	(SumC    [firstSC : ArithC]
			 [secondSC : ArithC]) 
	(MultC   [firstMC : ArithC]
			 [secondMC : ArithC]))

(define-type ArithS
	(NumberS [num : Number])
	(SumS    [firstSS : ArithS]
			 [secondSS : ArithS])
	(MultS   [firstMS : ArithS]
			 [secondMS : ArithS])
	(RestS   [firstRS : ArithS]
			 [secondRS : ArithS])
	(NegS    [numS : ArithS]))
	
(define (parse [expr : S-Exp]) : ArithS
  (cond 
	[(s-exp-match? `NUMBER expr) (NumberS (s-exp->number expr))]
	[(s-exp-match? `(ANY ANY ANY) expr) 
		(let ((ls (s-exp->list expr)))
		 (case (s-exp->symbol (first ls)) 
		   ((+) (SumS (parse (second ls)) (parse (third ls))))
		   ((-) (RestS (parse (second ls)) (parse (third ls))))
		   ((*) (MultS (parse (second ls)) (parse (third ls))))
		   (else (error 'parse "expresión aritmética malformada"))))]
	[(s-exp-match? `(- ANY) expr) (NegS (parse (second (s-exp->list expr))))]
	[else (error 'parse "expresión aritmética malformada")]))

(define (de-sugar [expS : ArithS]) : ArithC
  (cond
	[(NumberS? expS) (NumberC (NumberS-num expS))]
	[(SumS? expS) (SumC (de-sugar (SumS-firstSS expS)) (de-sugar (SumS-secondSS expS)))]
	[(MultS? expS) (MultC (de-sugar (MultS-firstMS expS)) (de-sugar (MultS-secondMS expS)))]
	[(RestS? expS) (SumC (de-sugar (RestS-firstRS expS)) (MultC (NumberC -1) (de-sugar (RestS-secondRS expS))))]
	[(NegS? expS) (MultC (NumberC -1) (de-sugar (NegS-numS expS)))]))

(define (interp [expC : ArithC]) : Number
  (cond
	[(NumberC? expC) (NumberC-num expC)]
	[(SumC? expC) (+ (interp (SumC-firstSC expC)) (interp (SumC-secondSC expC)))]
	[(MultC? expC) (* (interp (MultC-firstMC expC)) (interp (MultC-secondMC expC)))]))
		   
(define (eval [exp : S-Exp])
  (interp (de-sugar (parse exp))))
