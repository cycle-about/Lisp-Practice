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
			((eq? e '#t) #t)
			((eq? e '#f) #f)
			(else (build 'primitive e))
		)
	)
)

(define atom-to-action
	(lambda (e)
		(cond
			((number? e) *const)
			((eq? e '#t) *const)
			((eq? e '#f) *const)
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
			(else *identifier) ; this does not run, because it's not a procedure
		)
	)
)

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

; p 184-5
; handles "cond..."" blocks: read each line, if left part is false, look at rest of lines. Otherwise, answer the right part. else is treated as true
; this does violate the First Commandment, since there is no check of "null? lines", so at least one of the questions needs to be true
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
			((atom? x) (eq? x 'else))
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
    [else *application]))

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

; p 186
; takes a list of (representations of) arguments, and a table
; returns a list composed of the meaning of each argument
(define evlis
	(lambda (args table)
		(cond
			((null? args) '() )
			(else (cons (meaning (car args) table) (evlis (cdr args) table)))
		)
	)
)

(define function-of car) ; p 187
(define arguments-of cdr) ; p 187

; p 186
; An application is a list of expressions whose car position contains an expression whose value is a function
; it must always determine the meaning of all its arguments
(define *application
	(lambda (e table)
		(apply
			(meaning (function-of e) table)
			(evlis (arguments-of e) table)
		)
	)
)

; we know what primitives functions do (p 184)
(define primitive?
	(lambda (l)
		(eq? (first l) 'primitive)
	)
)

; non-primitive functions are defined by their arguments and function bodies, need those two things to use it (p 184)
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

; p 188-9
; this function must extend the table
; a closure is a non-primitive function
; applying a non-primitive function (a closure) to a list of values is the same as finding the meaning of the closure's body with its table extended by an entry in the form "(formals values)". In this entry, formals is the formals of the closure and values is the result of evlis
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

;; p 184
;; (define e1 '(lambda (x) (cons x y))) ; quote statements to be sent to the interpreter to process, rather than being executed
;; (define t1 '(((y z) ((8) 9))))
; (displayln (meaning e1 t1))  ; (non-primitive ((((y z) ((8) 9))) (x) (cons x y)))

;; p 185
(define e2 '(
	cond 
		(coffee klatsch) 
		(else party)
))

(define t2 '(
	((coffee) (#t)) 
	((klatsch party) 
	(5 (6)))
))

; (displayln (*cond e2 t2)) ; 5

;; p 189
(define c '(
	(
		((u v w) (1 2 3))
		((x y z) (4 5 6))
	)
	(x y)
	(cons z x)
))

(define v1 '(
	(a b c)
	(d e f)
))

; (displayln (apply-closure c v1)) ; (6 a b c)

;; p 189
(define e3 '(cons z x))
(define t3 '(
	((x y) ((a b c) (d e f))) 
	((u v w) (1 2 3)) 
	((x y z) (4 5 6))
))
; (displayln (meaning e3 t3)) ; (6 a b c)

;; p 190
(define z 6)
(define x '(a b c))
; (displayln (cons z x)) ; (6 a b c)

;; p 190
(define a '(z x))
(displayln (evlis a t3)) ; (6 (a b c))

; that comes from these pieces, because evlis returns a list of meanings
(define e4 'z)
(displayln (meaning e4 t3)) ; 6
(define e5 'x)
(displayln (meaning e5 t3)) ; (a b c)

;; p 190
(define e6 'cons)
(displayln (meaning e6 t3)) ; (primitive cons)

;; p 191
(define f '(primitive cons))
(define v2 '(6 (a b c)))
(displayln (apply f v2)) ; (6 a b c)

;; p 191
(define n 'cons)
(displayln (apply-primitive n v2)); (6 a b c)