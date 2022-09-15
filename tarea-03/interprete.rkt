#lang plait

(define-type Value
  (numV [n : Number])
  (strV [str : String])
  (idV [id : Symbol])
  (boolV [b : Boolean]))

(define-type Ops
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

(define-type ExprC
  (numC [n : Number])
  (boolC [b : Boolean])
  (strC [str : String])
  (idC [id : Symbol])
  (ifC [pred : ExprC]
	   [true-expr : ExprC]
	   [false-expr : ExprC])
  (binopC [op : Ops]
		  [first : ExprC]
		  [second : ExprC])
  (funC [arg : Symbol]
		[body : ExprC])
  (appC [first : ExprC]
		[second : ExprC]))

(define (desugar [s : ExprS]) : ExprC
  (cond
	([numS? s] (numC (numS-n s)))
	([boolS? s] (boolC (boolS-b s)))
	([strS? s] (strC (strS-str s)))
	([idS? s] (idC (idS-id s)))
	([ifS? s] (ifC (desugar (ifS-pred s)) (desugar (ifS-true-expr s)) (desugar (ifS-false-expr s))))
	([andS? s] (ifC (desugar (andS-first s)) (desugar (andS-second s)) (boolC #f)))
	([orS? s] (ifC (desugar (orS-first s)) (boolC #t) (desugar (orS-second s))))
	([binopS? s] (binopC (binopS-op s) (desugar (binopS-first s)) (desugar (binopS-second s))))
	([funS? s] (funC (funS-arg s) (desugar (funS-body s))))
	([letS? s] (appC (funC (letS-id s) (desugar (letS-body s))) (desugar (letS-val s))))
	([appS? s] (appC (desugar (appS-first s)) (desugar (appS-second s))))))


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


;(define (eval [str : S-Exp]) : Value
;  (interp (desugar (parse str))))

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
	 (parse-id in)]))

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
	  (andS (parse (second inlst)) (parse (third inlst)))
	  (error 'parse "cantidad incorrecta de argumentos para and"))))

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

