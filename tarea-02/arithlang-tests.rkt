#lang plait

(require "arithlang.rkt")

(test (parse `(+ 1 5)) 
	  (SumS (NumberS 1) (NumberS 5)))

(test (parse `5)
	  (NumberS 5))

(test (parse `(* (+ 1 5) 5))
	  (MultS (SumS (NumberS 1) (NumberS 5)) (NumberS 5))) 
(test (parse `(- 5 1))
	  (RestS (NumberS 5) (NumberS 1)))

(test (parse `(- 5))
	  (NegS (NumberS 5)))

;Tests for sum

(test (eval `1) 1)
(test (eval `-1) -1)

(test (eval `(+ 1 5)) 6)
(test (eval `(+ -1 6)) 5)
(test (eval `(+ (- 1) 6)) 5)

(test/exn (eval `(+ 1 1 1)) "expresión aritmética malformada")

(test (eval `(+ (+ 1 5) -1)) 5)
(test (eval `(+ (+ 1 5) (+ 5 5))) 16)

;Tests for mult

(test (eval `(* 1 5)) 5)
(test (eval `(* (* 5 5) 5)) 125)
(test (eval `(* (* 2 3) (* 4 5))) 120)

;Tests for rest

(test (eval `(- 5 1)) 4)
(test (eval `(- (- 5 1) (- 6 (- -2)))) 0)

;Tests for neg

(test (eval `(- 5)) -5)
(test (eval `(- -5)) 5)
(test (eval `(- (* 4 3))) -12)
