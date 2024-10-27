#lang racket

; Chapter 3. Cons the Magnificent

#|

; version that removes matching word and everything before it
(define rember-1
	(lambda (a lat)
		(cond
			((null? lat) (displayln '() )) 		; if lat is empty, print an empty list
			(else (cond 											; else lat not empty
				((eq? (car lat) a) (cdr lat))		; if car lat equals a: return cdr lat
				(else (rember-1 a (cdr lat))) 	; else car lat not equals a: call rember-1 on cdr lat
			))
		)
	)
)

; corrected version
(define rember-2
	(lambda (a lat)
		(cond
			((null? lat) (displayln '() ))   										; if lat is empty, print an empty list
			(else (cond
				((eq? (car lat) a) (cdr lat)) 										; if car of lat equals a: return cdr lat
				(else (cons (car lat) (rember-2 a (cdr lat))))  	; if car lat not equals a: add car lat to rember a with cdr lat (pick up car lat at end)
			))
		)
	)
)

; simplified version that does the same as 2
(define rember-3
	(lambda (a lat)
		(cond
			((null? lat) (displayln '() )) 										; second part has to be a function, empty list throws error
			((eq? (car lat) a) (cdr lat)) 										; if car lat equals a: return cdr lat
			(else (cons (car lat) (rember-3 a (cdr lat)))) 		; else keep cdr lat to add onto result of rember-3 a with cdr lat
		)
	)
)

(displayln (rember-3 "bacon" '() )) 																; #<void>   [after the print statement by function]
(displayln (rember-3 "bacon" '("bacon" "lettuce" "and" "tomato"))) 	; (lettuce and tomato)
(displayln (rember-3 "and" '("bacon" "lettuce" "and" "tomato"))) 		; (bacon lettuce tomato)


(define firsts
	(lambda (l)
		(cond
			((null? l) '() ) 																; this one has empty list in second part
			(else (cons (car (car l)) (firsts (cdr l)))) 		; get the car of the car of l, add it to first on cdr l
		)
	)
)

(displayln (firsts '(("a" "b") ("c" "d") ("e" "f")) )) 		; (a c e)


; adds 'new' to the right of 'old' in a list
(define insertR
	(lambda (new old lat)
		(cond
			((null? lat) (displayln '() ))
			(else (cond
				((eq? (car lat) old) (cons old (cons new (cdr lat))))     ; if car lat equals old: get old, add to cons of new onto (cdr lat)
					(else (cons (car lat) (insertR new old (cdr lat)))) 		; else: get car lat, add to insertR called on new old (cdr lat)
			))
		)
	)
)

(define new "topping")
(define old "fudge")
(define lat '("ice" "cream" "with" "fudge" "for" "dessert"))   ; (ice cream with fudge topping for dessert)
(displayln (insertR new old lat))

; adds 'new' to the left of 'old' in a list
; difference is order of cons in the matching case
(define insertL-1
	(lambda (new old lat)
		(cond
			((null? lat) (displayln '() ))
			(else (cond
				((eq? (car lat) old) (cons new (cons old (cdr lat)))) 		; if car lat equals old: get new, add it to result of old cons to cdr lat
					(else (cons (car lat) (insertL-1 new old (cdr lat)))) 		; else: get car lat, add to result of insertL-1 on new old cdr lat
			))
		)
	)
)

; different way to do insertL
; "(cons new (cons old (cdr lat))" is the same as "cons new lat", because there it's true that car lat equals old
(define insertL-2
	(lambda (new old lat)
		(cond
			((null? lat) (displayln '() ))
			(else (cond
				((eq? (car lat) old) (cons new lat)) 												; if car lat equals old: get new, add it to current list
					(else (cons (car lat) (insertL-2 new old (cdr lat)))) 		; else: get car lat, add to result of insertL-1 on new old cdr lat
			))
		)
	)
)

(define new "topping")
(define old "fudge")
(define lat '("ice" "cream" "with" "fudge" "for" "dessert"))   ; (ice cream with topping fudge for dessert)
(displayln (insertL-2 new old lat))

(define subst-1
	(lambda (new old lat)
		(cond
			((null? lat) (displayln '() ))
			(else (cond
				((eq? (car lat) old) (cons new (cdr lat)))
				(else (cons (car lat) (subst-1 new old (cdr lat))))
			))
		)
	)
)

(displayln (subst-1 "fudge" "topping" '("ice" "cream" "with" "topping" "for" "dessert") )) 		; (ice cream with fudge for dessert)


; substitute 'new' for the first occurrence of either 'o1' or 'o2'
(define subst-2
	(lambda (new o1 o2 lat)
		(cond
			((null? lat) (displayln '() ))
			(else (cond
				((or (eq? (car lat) o1) (eq? (car lat) o1)) (cons new (cdr lat)))   ; if car lat equals o1 or o2, cons new onto cdr lat
				(else (cons (car lat) (subst-2 new o1 o2 (cdr lat))))
			))
		)
	)
)

(define new "vanilla")
(define o1 "chocolate")
(define o2 "banana")
(define lat '("banana" "ice" "cream" "with" "chocolate" "topping"))
(displayln (subst-1 new o2 lat)) 							; (vanilla ice cream with chocolate topping)
(displayln (subst-2 new o1 o2 lat))  					; (banana ice cream with vanilla topping)      * subst-2 found the 'end' value first
; it finds the END value first

(define multirember
	(lambda (a lat)
		(cond
			((null? lat) displayln '() )
			(else
				(cond
					((eq? (car lat) a) (multirember a (cdr lat))) 			; if first item of lat equals a: call func on remainder of list (remove item)
					(else (cons (car lat) (multirember a (cdr lat)))) 	; if not: 'keep' that first item, and call func on remainder of list
				)
			)
		)
	)
)

(displayln (multirember "cup" '("coffee" "cup" "tea" "cup" "hick" "cup") )) 		; (coffee tea hick)

(define multiinsertR
	(lambda (new old lat)
		(cond
			((null? lat) displayln '() )
			(else
				(cond
					((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
					(else (cons (car lat) (multiinsertR new old (cdr lat))))
				)
			)
		)
	)
)

; if first item in lat equals old: store first item, store new, then call func on old new and remainder of list
; if not: store first item, call func on same thing as true case

(displayln (multiinsertR "fried" "fish" '("chips" "and" "fish" "or" "fish" "and" "fried") ))
	; (chips and fish fried or fish fried and fried)

(define multiinsertL
	(lambda (new old lat)
		(cond
			((null? lat) displayln '() )
			(else
				(cond
					((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
					(else (cons (car lat) (multiinsertL new old (cdr lat))))
				)
			)
		)
	)
)

(displayln (multiinsertL "fried" "fish" '("chips" "and" "fish" "or" "fish" "and" "fried") ))
	; (chips and fried fish or fried fish and fried)

|#

(define multisubst
	(lambda (new old lat)
		(cond
			((null? lat) displayln '() )
			(else
				(cond
					((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
					(else (cons (car lat) (multisubst new old (cdr lat))))
				)
			)
		)
	)
)

(displayln (multisubst "hi" "hello" '("hello" "world") ))