#lang racket

;;; Question: why does Example 1 not print out "void" and "list is empty" with non-empty inputs, while the similar Example 2 does?

;; Example 1. Returns list with an item removed
(define rember-3
	(lambda (a lat)
		(cond
			((null? lat) '() ) 					; return empty list, means gets ignored as intended
			; ((null? lat) (displayln '("list is empty") )) 			; if lat is null: display list is empty
			((eq? (car lat) a) (cdr lat)) 										; if car lat equals a: return cdr lat
			(else (cons (car lat) (rember-3 a (cdr lat)))) 		; if neither of those: store car lat, add it to result of rember-3 a with cdr lat
		)
	)
)

(displayln '("############################# "))
(displayln '("#### Results of rember-3 ####"))
(displayln (rember-3 "bacon" '("bacon" "lettuce" "and" "tomato"))) 	; (lettuce and tomato)
(displayln (rember-3 "and" '("bacon" "lettuce" "and" "tomato"))) 		; (bacon lettuce tomato)

; how to intentionally get the void message from this function
(displayln (rember-3 "bacon" '() )) 																; (list is empty) #<void>



;; Example 2. Returns a list with numbers removed

(define no-nums
	(lambda (lat)
		(cond
			((null? lat) '() ) 					; return empty list, means gets ignored as intended
			; ((null? lat) (displayln '("list is empty") ) 				; this statement gets 'cons' to the result, not what's intended
			(else 																								; if not null, continue
				(cond 
					((number? (car lat)) (no-nums (cdr lat)))  				; if car lat is number: run no-nums on cdr lat
					(else (cons (car lat) (no-nums (cdr lat))))  			; if not: add car lat to result of no-nums run on cdr lat
				)
			)
		)
	)
)

(displayln '("############################ "))
(displayln '("#### Results of no-nums #### "))
(displayln (no-nums '("bacon" "lettuce" "and" "tomato"))) 		; only strings 					--> (list is empty) (bacon lettuce and tomato . #<void>)
(displayln (no-nums '("bacon" "lettuce" "and" "tomato" 3))) 	; both 								 	--> (list is empty) (bacon lettuce and tomato . #<void>)
(displayln (no-nums '(3 "bacon" "lettuce" "and" "tomato" ))) 	; both 								 	--> (list is empty) (bacon lettuce and tomato . #<void>)
(displayln (no-nums '(5 6 9))) 															; only numbers 					--> (list is empty) #<void>
(displayln (no-nums '() ))  																	; empty list 						--> (list is empty) #<void>

; how resolved
; return an empty list (instead of void, or f, or a print statement)