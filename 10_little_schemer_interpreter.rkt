#lang racket

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

;; from chapter 9
(define atom? 
	(lambda (x) 
		(and (not (pair? x)) (not (null? x))))
)

;; from chapter 7
;; used to make representation of pairs
;; does not enforce a type check that args are sets
(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '() ))     ; empty list used to initialize and empty list, for set2 to be added onto: lists are immutable, so need new list
	)
)

;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; entry: a pair of lists whose first list is a set, and the two lists are the same length
;; FUNCTION 1
(define lookup-in-entry
	(lambda (name entry entry-f)
		(lookup-in-entry-help name 
			(first entry)
			(second entry)
			entry-f
		)
	)
)

(define new-entry build)
(define extend-table cons)

;; FUNCTION 2
;; this takes an additional argument invoked when name is not found in first list of entry, called entry-f
;; means what to do in that case is left up to caller of the function

(define lookup-in-entry-help
	(lambda (name names values entry-f)
		(cond
			((null? names) (entry-f name))
			((eq? (car names) name)
				(car values))
			(else 
				(lookup-in-entry-help name (cdr names) (cdr values) entry-f)
			)
		)
	)
)

(define entry-f (lambda (name) 'not-found))

(define lookup-in-table
	(lambda (name table table-f)
		(cond
			((null? table) (table-f name))
			(else
				(lookup-in-entry name (car table)
					(lambda (name) (lookup-in-table name (cdr table) table-f))
				)
			)
		)
	)
)

(define table-f (lambda (name) 'not-found-table))

(define *const
	(lambda (e table)
		(cond
			((number? e) e)
			((eq? e #t) #t)
			((eq? e #f) #f)
			(else (build 'primitive e))
		)
	)
)

(define atom-to-action
	(lambda (e)
		(cond
			((number? e) *const)
			((eq? e #t) *const)
			((eq? e #f) *const)
			((eq? e 'cons) *const)
			((eq? e 'car) *const)
			((eq? e 'cdr) *const)
			((eq? e 'null?) *const)
			((eq? e 'eq?) *const)
			((eq? e 'atom?) *const)
			((eq? e 'zero?) *const)
			((eq? e 'add1) *const)
			((eq? e 'sub1) *const)
			((eq? e 'number?) *const)
			(else '*identifier)
		)
	)
)

#|
(displayln "function 15")
(displayln (atom-to-action 6)) 				; #<procedure:*const>
(displayln (atom-to-action #f)) 			; #<procedure:*const>
(displayln (atom-to-action 'car)) 		; #<procedure:*const>
(displayln (atom-to-action 'x)) 			; *identifier
|#

(define text-of second)
(define table-of first)
(define formals-of second)
(define body-of third)

(define *quote
	(lambda (e table)
		(text-of e)
	)
)

(define initial-table
	(lambda (name)
		(car '() )
	)
)


(define *identifier
	(lambda (e table)
		(lookup-in-table e table initial-table)
	)
)

(define *lambda
	(lambda (e table)
		(build 'non-primitive (cons table (cdr e)))
	)
)

(define cond-lines-of cdr)

(define *cond
	(lambda (e table)
		(evcon (cond-lines-of e) table)
	)
)

(define evcon
	(lambda (lines table)
		(cond
			((else? (question-of (car lines))) 
				(meaning (answer-of (car lines)) table))
			((meaning (question-of (car lines)) table)
				(meaning (answer-of (car lines)) table))
			(else (evcon (cdr lines) table))
		)
	)
)

(define else?
	(lambda (x)
		(cond
			((atom? x) (eq? x ('else)))
			(else #f)
		)
	)
)

(define question-of first)
(define answer-of second)


(define (list-to-action e)
  (cond
    [(atom? (car e))
     (cond
       [(eq? (car e) 'quote) *quote]
       [(eq? (car e) 'lambda) *lambda]
       [(eq? (car e) 'cond)   *cond]
       [else                  *application])]
    [else '*application]))



(define expression-to-action
	(lambda (e)
		(cond
			((atom? e) (atom-to-action e))
			(else (list-to-action e))
		)
	)
)


(define meaning
	(lambda (e table)
		((expression-to-action e) e table)
	)
)


(define value
	(lambda (e)
		(meaning e '() ) ; empty table
	)
)

(define evlis
	(lambda (args table)
		(cond
			((null? args) '() )
			(else (cons (meaning (car args) table) (evlis (cdr args) table)))
		)
	)
)

(define function-of car)
(define arguments-of cdr)

(define *application
	(lambda (e table)
		(apply
			(meaning (function-of e) table)
			(evlis (arguments-of e) table)
		)
	)
)

(define primitive?
	(lambda (l)
		(eq? (first l) 'primitive)
	)
)

(define non-primitive?
	(lambda (l)
		(eq? (first l) 'non-primitive)
	)
)

;; if fun does not evaluate to either primitive or non-primitive, there is no answer (p 187)
(define apply
	(lambda (fun vals)
		(cond
			((primitive? fun) (apply-primitive (second fun) vals))
			((non-primitive? fun) (apply-closure (second fun) vals))
		)
	)
)

(define :atom?
	(lambda (x)
		(cond
			((atom? x) #t)
			((null? x) #f)
			((eq? (car x) 'primitive) #t)
			((eq? (car x) 'non-primitive) #t)
			(else #f)
		)
	)
)

(define apply-primitive
	(lambda (name vals)
		(cond
			((eq? name 'cons) (cons (first vals) (second vals)))
			((eq? name 'car) (car (first vals)))
			((eq? name 'cdr) (cdr (first vals)))
			((eq? name 'null?) (null? (first vals)))
			((eq? name 'eq?) (eq? (first vals) (second vals)))
			((eq? name 'atom?) (:atom? (first vals)))
			((eq? name 'zero?) (zero? (first vals)))
			((eq? name 'add1) (add1 (first vals)))
			((eq? name 'sub1) (sub1 (first vals)))
			((eq? name 'number?) (number? (first vals)))
		)
	)
)

(define apply-closure
	(lambda (closure vals)
		(meaning (body-of closure)
			(extend-table
				(new-entry (formals-of closure) vals)
				(table-of closure)
			)
		)
	)
)

;; page 184
(define ex8 '(lambda (x) (cons x y))) ; quote things intended for the interpreter to process
(define t4 '(((y z) ((8) 9))))
(displayln (meaning ex8 t4))  ; (non-primitive ((((y z) ((8) 9))) (x) (cons x y)))