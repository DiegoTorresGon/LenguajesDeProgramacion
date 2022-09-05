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

	(test-case "list->chunks"
		(check-equal? (list->chunks '(x y z a) 2)
					  '((x y) (z a)))
		(check-equal? (list->chunks '(a b c) 3)
					  '((a b c)))
		(check-equal? (list->chunks '(a b c) 5)
					  '((a b c)))
		(check-equal? (list->chunks '(a b c d) 3)
					  '((a b c) (d)))
		(check-equal? (list->chunks '() 3)
					  '()))

	(test-case "partition"
		(check-equal? (partition "abcd" 2)
					 (bundle (explode "abcd") 2))
		(check-equal? (partition "abcd" 3)
					  '("abc" "d")))

	(test-case "isort"
		(check-equal? (isort '(5 4 3 2 1) <)
					  '(1 2 3 4 5))
		(check-equal? (isort '(1 3 2 4 5) >)
					  '(5 4 3 2 1))
		(check-equal? (isort '("Bob" "Carlos" "Alejandro") string<?)
					  '("Alejandro" "Bob" "Carlos"))
		(check-equal? (isort '() >)
					  '()))
	
	(test-case "quicksort"
		(check-equal? (quicksort '(5 4 3 2 1) <)
					  '(1 2 3 4 5))
		(check-equal? (quicksort '(7 1 4 9 10 0) <)
					  '(0 1 4 7 9 10))
		(check-equal? (quicksort '() <)
					  '())
		(check-equal? (quicksort '(1 3 8 7 2 3 3) <)
					  '(1 2 3 3 3 7 8))
		(check-equal? (quicksort '("Bob" "Carlos" "Alejandro") string<?)
					  '("Alejandro" "Bob" "Carlos"))
		;This a test for stability:
		(check-equal? 
		  (quicksort '((1 . 4) (2 . 3) (1 . 7) (1 . 1))
					 (lambda (x y) 
					   (< (car x) (car y))))
					 '((1 . 4) (1 . 7) (1 . 1) (2 . 3)))))


(run-tests pruebas 'verbose)
