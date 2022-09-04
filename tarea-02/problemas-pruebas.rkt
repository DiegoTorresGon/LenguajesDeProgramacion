#lang racket

(require rackunit rackunit/text-ui
		 "problemas.rkt")

(define-test-suite pruebas

	(test-case "bundle"
		(check-equal? (bundle '("a" "b" "c" "d") 2)
					  '("ab" "cd"))
		(check-equal? (bundle '("a" "b" "c" "d") 3)
					  '("abc" "d"))
		(check-equal? (bundle '("a" "b" "c" "d") 7)
					  '("abcd"))
		(check-equal? (bundle '() 3)
					 '()))

	(test-case "partition"
		(check-equal? (partition "abcd" 2)
					 (bundle (explode "abcd") 2))
		(check-equal? (partition "abcd" 3)
					  '("abc" "d"))))

(run-tests pruebas 'verbose)



