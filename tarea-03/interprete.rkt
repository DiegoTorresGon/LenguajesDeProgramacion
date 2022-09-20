#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;				INTERP			   ;;;;;;	
;;;;;;;;;		De ExprC a valores 		   ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Value
  (numV [n : Number])
  (strV [str : String])
  (idV [id : Symbol])
  (boolV [b : Boolean])
  (funV [arg : Symbol]
		[body : ExprC]))

(define-type Ops
  (plusO)
  (appendO)
  (numeqO)
  (streqO))


(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str))))


; Definiendo un entorno de que acepte ligamientos de Symbolos a cualquiera de
; las variantes de Value.

(define-type Binding
  (binding [id : Symbol]
		   [value : Value]))

(define empty-env empty)

(define-type-alias Environment (Listof Binding))

(define (extend-env [id : Symbol] [value : Value] [env : Environment]) : Environment
  (cons (binding id value) env)) 

(define (lookup-id [id : Symbol] [env : Environment]) : Value
  (cond 
	([empty? env] (error 'lookup-id (string-append "identificador no está enlazado: " (to-string id))))
	([eq? id (binding-id (first env))] (binding-value (first env)))
	(else (lookup-id id (rest env)))))

(define (check-for-id [id : Symbol] [env : Environment]) : Boolean
  (cond 
	([empty? env] (error 'lookup-id (string-append "identificador no está enlazado: " (to-string id))))
	([eq? id (binding-id (first env))] #t)
	(else (check-for-id id (rest env)))))


(define (interp-num s) : Value
  (numV (numC-n s)))

(define (interp-bool s) : Value
  (boolV (boolC-b s)))

(define (interp-str s) : Value
  (strV (strC-str s)))

(define (interp-id [s : ExprC] [env : Environment]) : Value
  (lookup-id (idC-id s) env))

(define (interp-if [s : ExprC] [env : Environment]) : Value
  (let ([pred (interp-help (ifC-pred s) env)]) 
    (cond
  	([not (boolV? pred)] (if (ifC-bool-if s)
  						   (error 'interp "operación lógica con argumento que no es un valor booleano")
  						   (error 'interp "if tiene un condicional que no es de tipo booleano")))
  	([boolV-b pred] (let ([true-expr (interp-help (ifC-true-expr s) env)])
  					  (if (and (ifC-bool-if s) (not (boolV? true-expr)))
  						(error 'interp "operación lógica con argumento que no es un valor booleano")
  						true-expr)))
  	(else (let ([false-expr (interp-help (ifC-false-expr s) env)])
  			(if (and (ifC-bool-if s) (not (boolV? false-expr)))
  			  (error 'interp "operación lógica con argumento que no es un valor booleano")
  			  false-expr))))))

(define (interp-binop [s : ExprC] [env : Environment]) : Value
  (let ([first (interp-help (binopC-first s) env)] 
  	  [second (interp-help (binopC-second s) env)]
  	  [op (binopC-op s)])
    (type-case Ops op
  	[(plusO) (if (and (numV? first) (numV? second))
  			   (numV (+ (numV-n first) (numV-n second)))
  			   (error 'interp (string-append "argumento incorrecto para la suma, se requieren número: "
											 (string-append (to-string first)
															(string-append ", y, " (to-string second))))))]
  	[(appendO) (if (and (strV? first) (strV? second))
  				 (strV (string-append (strV-str first) (strV-str second)))
  				 (error 'interp "argumento incorrecto para la concatenación, se requieren cadenas"))]
  	[(numeqO) (if (and (numV? first) (numV? second))
  				(boolV (= (numV-n first) (numV-n second)))
  				(error 'interp "argumento incorrecto para la igualdad, se requieren números"))]
  	[(streqO) (if (and (strV? first) (strV? second))
  				(boolV (string=? (strV-str first) (strV-str second)))
  				(error 'interp "argumento incorrecto para la igualdad de cadenas"))])))

(define (interp-fun [s : ExprC] [env : Environment]) : Value
  (if (check-unbound-id s env)
	(funV (funC-arg s) (funC-body s))
	(error 'interp "identificador no está enlazado")))

(define (check-unbound-id [s : ExprC] env) : Boolean
  (cond  
	([numC? s] #t)
  	([boolC? s] #t)  
	([strC? s] #t)
  	([idC? s] (check-for-id (idC-id s) env))
  	([ifC? s] (and (check-unbound-id (ifC-pred s) env)
				   (check-unbound-id (ifC-true-expr s) env)
				   (check-unbound-id (ifC-false-expr s) env)))
  	([binopC? s] (and (check-unbound-id (binopC-first s) env)
					  (check-unbound-id (binopC-second s) env)))
  	([funC? s] (check-unbound-id (funC-body s) (extend-env (funC-arg s) (numV 0) env)))
  	([appC? s] (and (check-unbound-id (appC-first s) env)
					(check-unbound-id (appC-second s) env)))))

(define (interp-app [s : ExprC] [env : Environment]) : Value
  (let ([first (interp-help (appC-first s) env)]
  	  [arg (interp-help (appC-second s) env)])
    (if (funV? first)
  	(interp-help (funV-body first) (extend-env (funV-arg first) arg env))
  	(error 'interp "Aplicación de valor que no es una función"))))

(define (interp-help [s : ExprC] [env : Environment]) : Value
  (cond  
	([numC? s] (interp-num s))
  	([boolC? s] (interp-bool s))  ([strC? s] (interp-str s))
  	([idC? s] (interp-id s env))
  	([ifC? s] (interp-if s env))
  	([binopC? s] (interp-binop s env))
  	([funC? s] (interp-fun s env))
  	([appC? s] (interp-app s env))))

(define (interp [s : ExprC]) : Value
  (interp-help s empty-env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;				DESUGAR			   ;;;;;;	
;;;;;;;;;	Interfaz entre ExprS y ExprC   ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprC
  (numC [n : Number])
  (boolC [b : Boolean])
  (strC [str : String])
  (idC [id : Symbol])
  (ifC [pred : ExprC]
	   [true-expr : ExprC]
	   [false-expr : ExprC]
	   [bool-if : Boolean])
  (binopC [op : Ops]
		  [first : ExprC]
		  [second : ExprC])
  (funC [arg : Symbol]
		[body : ExprC])
  (appC [first : ExprC] [second : ExprC]))

(define (desugar [s : ExprS]) : ExprC
  (type-case ExprS s
	[(numS n) (numC n)]
	[(boolS b) (boolC b)]
	[(strS str) (strC str)]
	[(idS id) (idC id)]
	[(ifS pred true-expr false-expr) (ifC (desugar pred) (desugar true-expr) (desugar false-expr) #f)]
	[(andS first second) (ifC (desugar first) (desugar second) (boolC #f) #t)]
	[(orS first second) (ifC (desugar first) (boolC #t) (desugar second) #t)]
	[(binopS op first second) (binopC op (desugar first) (desugar second))]
	[(funS arg body) (funC arg (desugar body))]
	[(letS id val body) (appC (funC id (desugar body)) (desugar val))]
	[(appS first second) (appC (desugar first) (desugar second))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;		PARSE/ExprS		;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  (numS [n : Number])
  (boolS [b : Boolean])
  (strS [str : String])
  (idS [id : Symbol])
  (ifS [pred : ExprS]
	   [true-expr : ExprS]
	   [false-expr : ExprS])
  (andS [first : ExprS]
		[second : ExprS])
  (orS [first : ExprS]
	   [second : ExprS])
  (binopS [op : Ops]
		  [first : ExprS]
		  [second : ExprS])
  (funS [arg : Symbol]
		[body : ExprS])
  (letS [id : Symbol]
		[val : ExprS]
		[body : ExprS])
  (appS [first : ExprS]
		[second : ExprS]))

(define (parse [in : S-Exp]) : ExprS
  (cond
	[(s-exp-number? in)
	 (parse-number in)]
	[(s-exp-string? in)
     (parse-string in)]
	[(s-exp-match? `true in)
	 (boolS #t)]
 	[(s-exp-match? `false in)
	 (boolS #f)]
	[(s-exp-match? `{if ANY ...} in)
	 (parse-if in)]
	[(s-exp-match? `{and ANY ...} in)
	 (parse-and in)]
	[(s-exp-match? `{or ANY ...} in)
	 (parse-or in)]
	[(s-exp-match? `{+ ANY ...} in)
 	 (parse-+ in)]
	[(s-exp-match? `{++ ANY ...} in)
	 (parse-++ in)]
 	[(s-exp-match? `{num= ANY ...} in)
	 (parse-num= in)]
	[(s-exp-match? `{str= ANY ...} in)
	 (parse-str= in)]
	[(s-exp-match? `{fun ANY ...} in)
	 (parse-fun in)]
	[(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
	[(s-exp-match? `{ANY ...} in)
	 (parse-app in)]
	[(s-exp-symbol? in)
	 (parse-id in)]
	[else (error 'parse "expresión malformada")]))

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 4)
	  (ifS (parse (second inlst))
		   (parse (third inlst))
		   (parse (fourth inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (andS (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (orS (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
	[(s-exp-match? `{fun SYMBOL ANY ...} in)
	 (let ([inlst (s-exp->list in)])
	   (if (equal? (length inlst) 3)
		 (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
		 (error 'parse "funciones deben tener solo un cuerpo")))]
	[(s-exp-match? `{fun ANY ...} in)
	 (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 3)
	  (letS
		(s-exp->symbol (first (s-exp->list (second inlst))))
		(parse (second (s-exp->list (second inlst))))
		(parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
	(if (equal? (length inlst) 2)
	  (appS (parse (first inlst)) (parse (second inlst)))
	  (error 'parse "cantidad incorrecta de argumentos en aplicación de funciones"))))

