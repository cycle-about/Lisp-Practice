#lang racket

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

;; from chapter 9
(define atom? 
	(lambda (x) 
		(and (not (pair? x)) (not (null? x))))
)

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

; (displayln (lookup-in-entry 'entree e1 entry-f)) ; tastes
; (displayln (lookup-in-entry 'hello e1 entry-f)) ; not-found

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

; (displayln (lookup-in-table 'entree t2 table-f)) ; boeuf
; (displayln (lookup-in-table 'entree t3 table-f)) ; spaghetti because searches the entries in order, returning first match

;; FUNCTION 4

(define rep-a 'a)
(define rep-b 'b)
(define rep-c 'c)

#| 
(displayln
	(cons rep-a
		(cons rep-b
			(cons rep-c '() )
		)
	)
) ; (a b c)
|#

;; FUNCTION 5

#|
DATA TYPE 							BOOK 							RACKET
atom 										sans serif 				single quote, eg 'a
function  							italic 						n/a
scheme special forms 		bold 							n/a

value: a function that returns the natural value of expressions
|#

(define rep-car 'car) ; an atom, because printed in sans serif
(define rep-quote 'quote) ; an atom, because printed in sans serif

;; implementation uses '() for the book's bolded special form "quote ()"
(define f5
  (cons rep-car
        (cons
         (cons rep-quote
               (cons
                (cons rep-a
                      (cons rep-b
                            (cons rep-c '())))
                '()))
         '())))

; (displayln f5) ; (car (quote (a b c)))

;; the implementations of value in chapter 6 are only able to handle arithmetic expressions and operators
;; instead of calling value, displayln seems to accomplish the expected answers from the book

#|
;; FUNCTION 6
(displayln "function 6")
;; quote is in bold so it means the scheme special form
(displayln (car (quote (a b c))))   	; a     prints the value
; (car (quote (a b c)))   						; 'a    prints the atom, not the value

;; FUNCTION 7
(displayln "function 7")
;; show the value of expression in function 6, after giving it a name
;; quote in sans serif, so an atom
;; does not work when try implementing as 'quote so might be result of book showing it in an assignment
;; also does not work if try putting the list in '()
(define ex1 (car (quote (a b c)))) 				; a 
; (define ex1 (car ('quote (a b c)))) 		; ERROR: a: unbound identifier
; (define ex1 (car ('quote (a b c)))) 		; ERROR: a: unbound identifier
(displayln ex1)
|#

;; FUNCTION 8
(define ex2 (quote (car (quote '(a b c)))) )
; (displayln ex2) 						; (car (quote (a b c)))       the expression, not value

;; FUNCTION 9
(define ex3 (add1 6))
; (displayln ex3)  ; 7

;; FUNCTION 10
(define ex4 6) 			
; (displayln ex4) 	; 6

;; FUNCTION 11
(define ex5 (quote nothing))
; (displayln ex5)   ; nothing

;; FUNCTION 12
(define ex6 
	((lambda (nothing)
		(cons nothing '() ) )
		(quote (from nothing comes something))
	)
)
; (displayln ex6) ; ((from nothing comes something))

;; FUNCTION 13
(define ex7
	((lambda (nothing)
		(cond
			(nothing (quote something))
			(else (quote nothing))))
		#t)
)
; displayln ex7) ; something
; changing last item to #f prints nothing


;; FUNCTION 14
;; racket is dynamically typed, and does not have a built-in type function
;; types in the examples: *const, *quote, *identifier, *lambda, *cond, *application
;; how to represent types: with functions, called 'actions'
;; actions are functions that 'do the right thing' when applied to the appropriate type of expression
;; value should find the type of expression passed, then use the associated action
;; odd error during this: accidentally added an inline comment with a colon instead of semi-colon, throws unbound identifier, was difficult to see
(define (list-to-action e)
  (cond
    [(atom? (car e))
     (cond
       [(eq? (car e) 'quote) '*quote]
       [(eq? (car e) 'lambda) '*lambda]
       [(eq? (car e) 'cond)   '*cond]
       [else                  '*application])]
    [else '*application]))

(displayln "function 14")
(displayln (list-to-action '(quote (a b))))   ; *quote
(displayln (list-to-action '(lambda (x) x)))  ; *lambda
(displayln (list-to-action '(cond (else 1)))) ; *cond
(displayln (list-to-action '(car '(a b))))    ; *application
(displayln (list-to-action '((lambda (x) x) 5))) ; *application
(displayln (list-to-action '(f 1 2 3))) 			; *application

;; FUNCTION 15
(define atom-to-action
	(lambda (e)
		(cond
			((number? e) '*const)
			((eq? e #t) '*const)
			((eq? e #f) '*const)
			((eq? e 'cons) '*const)
			((eq? e 'car) '*const)
			((eq? e 'cdr) '*const)
			((eq? e 'null?) '*const)
			((eq? e 'eq?) '*const)
			((eq? e 'atom?) '*const)
			((eq? e 'zero?) '*const)
			((eq? e 'add1) '*const)
			((eq? e 'sub1) '*const)
			((eq? e 'number?) '*const)
			(else '*identifier)
		)
	)
)

(displayln "function 15")
(displayln (atom-to-action 6)) ; *const
(displayln (atom-to-action #f)) ; *const
(displayln (atom-to-action 'car)) ; *const
(displayln (atom-to-action 'x)) ; *identifier

;; FUNCTION 16
(define expression-to-action
	(lambda (e)
		(cond
			((atom? e) (atom-to-action e))
			(else (list-to-action e))
		)
	)
)

(displayln "function 16")
(displayln (expression-to-action '(quote (a b)))) ; *quote
(displayln (expression-to-action 'car)) ; *const

;; FUNCTION 17
(define meaning
	(lambda (e table)
		((expression-to-action e) e table)
	)
)

;; FUNCTION 18
;; the function value, together with all the functions it uses, is an interpreter
(define value
	(lambda (e)
		(meaning e '() ) ; empty table
	)
)



