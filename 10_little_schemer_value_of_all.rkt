#lang racket

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;



;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; entry: a pair of lists whose first list is a set, and the two lists are the same length
(define e1 '((appetizer entree beverage) (food tastes good)))
(define e2 '((appetizer entree beverage) (beer beer beer)))
(define e3 '((beverage dessert) ((food is) (number one with us))))

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

(displayln (lookup-in-entry 'entree e1 entry-f)) ; tastes
(displayln (lookup-in-entry 'hello e1 entry-f)) ; not-found

;; FUNCTION 3

;; table (or environment): list of entries
(define t1 '() )

; table with 2 entries
(define t2 
	'(
		((appetizer entree beverage) (pate boeuf vin))
		e3
	)
)

(define t3
	'(
		((entree dessert) (spaghetti spumoni))
		((beverage dessert) ((food is) (number one with us)))
		e1
	)
)

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

(displayln (lookup-in-table 'entree t2 table-f)) ; boeuf
(displayln (lookup-in-table 'entree t3 table-f)) ; spaghetti because searches the entries in order, returning first match

;; FUNCTION 4

(define rep-a 'a)
(define rep-b 'b)
(define rep-c 'c)

(displayln
	(cons rep-a
		(cons rep-b
			(cons rep-c '() )
		)
	)
) ; (a b c)

;; FUNCTION 5

(define rep-car 'car)
(define rep-quote 'quote)

(cons rep-car
	(cons 
		(cons rep-quote
			(cons
				(cons rep-a
					(cons rep-b
						(cons rep-c 
							(quote '() )
						)
					)
				)
				(quote '() )
			)
		)
	(quote '() )
	)
)

(displayln (quote (a b c)))
; also expressions in the function
;   '(car (quote (a b c . '()) . '()) . '())
;   (a b c)
