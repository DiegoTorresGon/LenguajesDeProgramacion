#lang plait

;(print-only-errors #t)

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

(define-type ExprC
  (numC [n : Number])
  (boolC [b : Boolean])
  (plusC [l : ExprC] [r : ExprC])
  (multC [l : ExprC] [r : ExprC])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (zeropC [e : ExprC])
  (idC [name : Symbol])
  (letC [name : Symbol]
        [value : ExprC]
        [body : ExprC])
  (callC [name : Symbol] [arg : ExprC]))

(define (bad-arg-to-op-error [op : Symbol] [v : Value])
  (error 'interp
         (string-append
          "bad argument to operator "
          (string-append
           (to-string op)
           (string-append
            ": "
            (to-string v))))))

(define (bad-conditional-error [v : Value])
  (error 'interp
         (string-append
          "bad conditional to if expression: "
          (to-string v))))

(define (unbound-identifier-error [name : Symbol])
  (error 'interp
         (string-append
          "unbound identifier: "
          (to-string name))))

(define (undefined-function-identifier [name : Symbol])
  (error 'interp
		 (string-append
		   "undefined-function-identifier: "
		   (to-string name))))

;; (define-type-alias Environment (Symbol -> Value))

;; (define (empty-env [name : Symbol]) : Value
;;   (unbound-identifier-error name))

;; (define (lookup-env name env)
;;   (env name))

;; (define (extend-env name val env)
;;   (lambda (id)
;;     (if (eq? id name)
;;         val
;;         (env id))))

(define-type Binding
  (binding [name : Symbol]
           [value : Value]))

(define-type Function
  (function [name : Symbol]
			[arg : Symbol]
			[body : ExprC]))

(define-type-alias FEnvironment (Listof Function))

(define-type-alias Environment (Listof Binding))

(define empty-env empty)
(define empty-f-env empty)

(define (lookup-function name arg f-env)
  (if (empty? f-env)
	  (undefined-function-identifier name)
	  (if (eq? name (function-name (first f-env)))
		(first f-env)
		(lookup-function name arg (rest f-env)))))
		

(define (lookup-env name env)
  (if (empty? env)
      (unbound-identifier-error name)
      (if (eq? name (binding-name (first env)))
          (binding-value (first env))
          (lookup-env name (rest env)))))

(define (extend-env name value env)
  (cons (binding name value) env))

(define (extend-f-env name arg body f-env0)
  (cons (function name arg body) f-env0))

(define (interp [e : ExprC] [env : Environment] [f-env : FEnvironment]) : Value
  (type-case ExprC e
    [(numC n) (numV n)]
    [(boolC b) (boolV b)]
    [(plusC e1 e2)
     (let ([v1 (interp e1 env f-env)]
           [v2 (interp e2 env f-env)])
       (cond [(not (numV? v1))
              (bad-arg-to-op-error '+ v1)]
             [(not (numV? v2))
              (bad-arg-to-op-error '+ v2)]
             [else
              (numV (+ (numV-n v1) (numV-n v2)))]))]
    [(multC e1 e2)
     (let ([v1 (interp e1 env f-env)]
           [v2 (interp e2 env f-env)])
       (cond [(not (numV? v1))
              (bad-arg-to-op-error '* v1)]
             [(not (numV? v2))
              (bad-arg-to-op-error '* v2)]
             [else
              (numV (* (numV-n v1) (numV-n v2)))]))]
    [(ifC e1 e2 e3)
     (let ([v1 (interp e1 env f-env)])
       (cond [(not (boolV? v1))
              (bad-conditional-error v1)]
             [(boolV-b v1)
              (interp e2 env f-env)]
             [else
              (interp e3 env f-env)]))]
    [(zeropC e)
     (let ([v (interp e env f-env)])
       (cond [(not (numV? v))
              (bad-arg-to-op-error 'zero? v)]
             [else
              (boolV (= (numV-n v) 0))]))]
    [(idC name)
     (lookup-env name env)]
    [(letC name value body)
     (interp body (extend-env name (interp value env f-env) env) f-env)]
	[(callC name arg) (let ([f (lookup-function name arg f-env)])
						(interp (function-body f) (extend-env (function-arg f) (interp arg env f-env) empty-env) f-env))]))

(define-type ExprS
  (numS [n : Number])
  (boolS [b : Boolean])
  (plusS [l : ExprS] [r : ExprS])
  (multS [l : ExprS] [r : ExprS])
  (bminusS [l : ExprS] [r : ExprS])
  (uminusS [e : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (notS [e : ExprS])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (zeropS [e : ExprS])
  (idS [name : Symbol])
  (letS [name : Symbol]
        [value : ExprS]
        [body : ExprS]))

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(boolS b) (boolC b)]
    [(plusS e1 e2) (plusC (desugar e1) (desugar e2))]
    [(multS e1 e2) (multC (desugar e1) (desugar e2))]
    [(bminusS e1 e2) (plusC (desugar e1) (multC (numC -1) (desugar e2)))]
    [(uminusS e) (multC (numC -1) (desugar e))]
    [(orS e1 e2) (ifC (desugar e1) (boolC #t) (desugar e2))]
    [(andS e1 e2) (ifC (desugar e1) (desugar e2) (boolC #f))]
    [(notS e) (ifC (desugar e) (boolC #f) (boolC #t))]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [(zeropS e) (zeropC (desugar e))]
    [(idS name) (idC name)]
    [(letS name value body) (letC name (desugar value) (desugar body))]))

(define (parse-error e)
  (error 'parse (string-append "malformed-input: " (to-string e))))

(define (parse [in : S-Exp]) : ExprS
  (cond [(s-exp-number? in)
         (numS (s-exp->number in))]
        [(s-exp-boolean? in)
         (boolS (s-exp->boolean in))]
        [(s-exp-list? in)
         (let ([ls (s-exp->list in)])
           (cond [(empty? ls)
                  (parse-error ls)]
                 [else
                  (let ([tag (first ls)])
                    (cond [(s-exp-symbol? tag)
                           (case (s-exp->symbol tag)
                             [(+)
                              (if (= (length ls) 3)
                                  (plusS (parse (second ls))
                                         (parse (third ls)))
                                  (parse-error ls))]
                             [(*)
                              (if (= (length ls) 3)
                                  (multS (parse (second ls))
                                         (parse (third ls)))
                                  (parse-error ls))]
                             [(-)
                              (let ([len (length ls)])
                                (cond [(= len 2)
                                       (uminusS (parse (second ls)))]
                                      [(= len 3)
                                       (bminusS (parse (second ls))
                                                (parse (third ls)))]
                                      [else
                                       (parse-error ls)]))]
                             [(or)
                              (let ([len (length ls)])
                                (if (= len 3)
                                    (orS (parse (second ls))
                                         (parse (third ls)))
                                    (parse-error ls)))]
                             [(and)
                              (let ([len (length ls)])
                                (if (= len 3)
                                    (andS (parse (second ls))
                                          (parse (third ls)))
                                    (parse-error ls)))]
                             [(not)
                              (let ([len (length ls)])
                                (if (= len 2)
                                    (notS (parse (second ls)))
                                    (parse-error ls)))]
                             [(if)
                              (let ([len (length ls)])
                                (if (= len 4)
                                    (ifS (parse (second ls))
                                         (parse (third ls))
                                         (parse (fourth ls)))
                                    (parse-error ls)))]
                             [(zero?)
                              (let ([len (length ls)])
                                (if (= len 2)
                                    (zeropS (parse (second ls)))
                                    (parse-error ls)))]
                             [(let)
                              (let ([len (length ls)])
                                (if (= len 3)
                                    (let ([binding (second ls)]
                                          [body (third ls)])
                                      (if (s-exp-list? binding)
                                          (let ([binding (s-exp->list binding)])
                                            (if (= (length binding) 2)
                                                (let ([name (first binding)]
                                                      [value (second binding)])
                                                  (if (s-exp-symbol? name)
                                                      (letS (s-exp->symbol name)
                                                            (parse value)
                                                            (parse body))
                                                      (parse-error ls)))
                                                (parse-error ls)))
                                          (parse-error ls)))
                                    (parse-error ls)))])]
                          [else
                           (parse-error tag)]))]))]
        [(s-exp-symbol? in)
         (idS (s-exp->symbol in))]
        [else (parse-error in)]))

(define (eval [in : S-Exp]) : Value
  (interp (desugar (parse in)) empty-env empty-f-env))



(module+ test

  ;; num
  (test (eval `0) (numV 0))
  (test (eval `5) (numV 5))

  ;; plus
  (test (eval `(+ 0 0)) (numV 0))
  (test (eval `(+ 0 1)) (numV 1))
  (test (eval `(+ 1 0)) (numV 1))
  (test (eval `(+ (+ 1 2) (+ 3 4))) (numV 10))
  (test (eval `(+ 123 321)) (eval `(+ 321 123)))

  ;; mult
  (test (eval `(* 0 0)) (numV 0))
  (test (eval `(* 0 1)) (numV 0))
  (test (eval `(* 1 0)) (numV 0))
  (test (eval `(* 1 5)) (numV 5))
  (test (eval `(* 5 1)) (numV 5))
  (test (eval `(* 6 7)) (numV 42))
  (test (eval `(* 3 (* 37 6))) (numV 666))

  ;; bminus
  (test (eval `(- 5 0)) (numV 5))
  (test (eval `(- 0 5)) (numV -5))
  (test (eval `(- 1 2)) (numV -1))
  (test (eval `(- 14 14)) (numV 0))
  (test (eval `(- (- 1 2) (- 3 6))) (numV 2))

  ;; uminus
  (test (eval `(- 0)) (numV 0))
  (test (eval `(- 1)) (numV -1))
  (test (eval `(- (- 5))) (numV 5))
  (test (eval `(- (- (- 8)))) (numV -8))

  ;; mezcla
  (test (eval `(+ (- 5 (* 3 2)) (- (- 3 2)))) (numV -2))
  (test (eval `(* (* (* (* (* 9 9) (* 9 9))
                        (* (* 9 9) (* 9 9)))
                     (* (* (* 9 9) (* 9 9))
                        (* (* 9 9) (* 9 9))))
                  (* (* (* (* 9 9) (* 9 9))
                        (* (* 9 9) (* 9 9)))
                     (* (* (* 9 9) (* 9 9))
                        (* (* 9 9) (* 9 9))))))
        (numV 3433683820292512484657849089281))

  ;; malformadas
  (test/exn (eval `(+ 1 2 3)) "malformed-input")
  (test/exn (eval `(* 5)) "malformed-input")
  ;(test/exn (eval `hola) "malformed-input")
  (test/exn (eval `"hola") "malformed-input")
  (test/exn (eval `(1 2 3)) "malformed-input")
  (test/exn (eval `(* (+ 1 2) (- 3 4) (- 5))) "malformed-input")


  ;; callC
  
  (test (interp (callC 'f (numC 5)) empty-env (extend-f-env 'f 'x (numC 3) empty-f-env)) 
		(numV 3))

  (test (interp (callC 'f (numC 5)) empty-env (extend-f-env 'f 'x (plusC (numC 3) (idC 'x)) empty-f-env)) 
		(numV 8))

  )
