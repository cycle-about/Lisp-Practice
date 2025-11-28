#lang racket

;;; Chapter 8. Lambda the Ultimate

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

(define (print-type obj)
	(printf "~a - " obj)
  (cond
    [(number? obj) (printf "Number\n")]
    [(string? obj) (printf "String\n")]
    [(boolean? obj) (printf "Boolean\n")]
    [(symbol? obj) (printf "Symbol\n")]
    [(list? obj) (printf "List\n")]
    [(procedure? obj) (printf "Procedure\n")]
    [else (printf "Unknown type\n")]))


(define addition
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (add1 (addition n (sub1 m))))
		)
	)
)


(define multiply
	(lambda (n m)
		(cond
			((zero? m) 0)
			(else (addition n (multiply n (sub1 m))))
		)
	)
)


; returns operator from prefix arithmetic expression (chapter 7)
(define operator
	(lambda (aexp)
		(car aexp)
	)
)


; helper function for prefix notation (chapter 6)
(define 1st-sub-exp
 	(lambda (aexp)
 		(car (cdr aexp))
 	)
)


; helper function for prefix (and infix) notation (chapter 6)
(define 2nd-sub-exp
	(lambda (aexp)
		(car (cdr (cdr aexp)))
	)
)


;; chapter 2
(define (atom? x)
	(not (list? x)))


;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; FUNCTION 1
;; version of rember that takes the equality tets as an arg (eq?, equal?, or =)
(define rember-f1
	(lambda (test? a l)
		(cond
			((null? l) '() )
			((test? (car l) a) (cdr l))
			(else (cons (car l) (rember-f1 test? a (cdr l))))
		)
	)
)

(define t1 =)
(define a1 5)
(define l1 '(6 2 5 3) )

; (displayln (rember-f1 t1 a1 l1))  ; (6 2 3)


;; Curry-ing: a function that when passed an argument a, returns another function with a as its arg, and tests for whether they are equal

;; FUNCTION 2
(define eq?-c
	(lambda (a)
		(lambda (x)
			(eq? x a)
		)
	)
)

; (print-type (eq?-c 3))   ; #<procedure:..._schemer_lambda.rkt:43:16> - Procedure
; [printout does not include name of function, only line numbers and file name]


;; FUNCTION 3
(define rember-f2
	(lambda (test?)
		(lambda (a l)
			(cond
				((null? l) '() )
				((test? (car l) a) (cdr l))
				(else (cons (car l) ((rember-f2 test?) a (cdr l))))
			)
		)
	)
)

(define a2 "tuna")
(define l2 '("shrimp" "salad" "and" "tuna" "salad") )
; (displayln ((rember-f2 eq?) a2 l2))     ; (shrimp salad and salad)


;; FUNCTION 4
;; make a version of insertL that also takes the equality function as an arg
; original insertL in chapter 3
(define insertL-f
	(lambda (test?)
		(lambda (new old l)
			(cond
				((null? l) '() )
				((test? (car l) old) (cons new (cons old (cdr l))))
				(else (cons (car l) ((insertL-f test?) new old (cdr l))))
			)
		)
	)
)

(define l3 '("chips" "and" "fish" "or" "fish" "and" "fried"))
(define n1 "fried")
(define o1 "fish")
; (displayln ((insertL-f eq?) n1 o1 l3))  ; (chips and fried fish or fish and fried)


;; FUNCTION 5
(define insertR-f
	(lambda (test?)
		(lambda (new old l)
			(cond
				((null? l) '() )
				((test? (car l) old) (cons old (cons new (cdr l))))
				(else (cons (car l) ((insertR-f test?) new old (cdr l))))
			)
		)
	)
)

; (displayln ((insertR-f eq?) n1 o1 l3))  ; (chips and fish fried or fish and fried)

;; FUNCTION 6
;; helper function that defines where the cons is different in insertL from insertR
(define seqL
	(lambda (new old l)
		(cons new (cons old l))
	)
)

;; FUNCTION 7
;; helper function for the cons in insertR
(define seqR
	(lambda (new old l)
		(cons old (cons new l))
	)
)

;; FUNCTION 8
(define insert-g
	(lambda (seq)
		(lambda (new old l)
			(cond
				((null? l) '() )
				((eq? (car l) old) (seq new old (cdr l)))
				(else (cons (car l) ((insert-g seq) new old (cdr l))))
			)
		)
	)
)

; (displayln ((insert-g seqL) n1 o1 l3))  ; (chips and fried fish or fish and fried)


;; FUNCTION 9
(define insertL-1 
	(insert-g 
		seqL 					; this is a named function, in place of a lambda
	)
)

; (displayln (insertL-1 n1 o1 l3))  ; (chips and fried fish or fish and fried)


;; it is not necessary to define functions to pass them as args, can pass the definitions themselves (page 132)
;; eg the arg seqL versus seqR is the only difference between revised insertL and insertR, so not useful as standalone functions


;; FUNCTION 10
;; insertL with an anonymous function, instead of seqL
(define insertL-2
	(insert-g
		(lambda (new old l)				; these three lines are a lambda (anonymous function), instead of named function 'seqL' as in insertL-1
			(cons new (cons old l))
		)
	)
)

; (displayln (insertL-2 n1 o1 l3))  ; (chips and fried fish or fish and fried)

;; FUNCTION 11
;; insertR with anonymous function
(define insertR-2
	(insert-g
		(lambda (new old l)				; these three lines are a lambda (anonymous function), instead of named function
			(cons old (cons new l))
		)
	)
)

;  (displayln (insertR-2 n1 o1 l3))  ; (chips and fish fried or fish and fried)

;; FUNCTION 12
;; helper function for substitute, to use the same calling function as inertL and insertR
(define seqS
	(lambda (new old l)
		(cons new l)
	)
)

;; FUNCTION 13
(define subst (insert-g seqS))

; (displayln (subst n1 o1 l3))  ; (chips and fried or fish and fried)


;; FUNCTION 14
(define rember
	(lambda (a l)
		((insert-g seqrem) #f a l) 		; false is there as a placeholder, because in this case there is no 'new' value
	)
)

;; FUNCTION 15
;; the function for what to do in function rember if (eq? (car l) old)
(define seqrem
	(lambda (new old l)
		l 											; just return the list, with neither the new or old item
	)
)

(define a3 "sausage")
(define l4 '("pizza" "with" "sausage" "and" "bacon") )

; (displayln (rember a3 l4))  ; (pizza with and bacon)

;; FUNCTION 16
;; 1. takes one argument y (not x due to similarity to the operator)
;; 2. returns the function indicated by the arg
(define atom-to-function
	(lambda (y)
		(cond
			((eq? y '+) addition)
			((eq? y 'x) multiply)
			(else expt)
		)
	)
)

(define nexp1 '(+ 5 3))
; (displayln (atom-to-function (operator nexp1)))  ; #<procedure:addition>

(define nexp2 '(x 5 3))
; (displayln (atom-to-function (operator nexp2)))  ; #<procedure:multiply>

(define nexp3 '(- 5 3))
; (displayln (atom-to-function (operator nexp3)))  ; #<procedure:expt>   [executed else]

(define nexp4 '(a 5 3))
; (displayln (atom-to-function (operator nexp4)))  ; #<procedure:expt>   [does not check arithmetic expression is valid, executed else]


;; FUNCTION 17
;; value using atom-to-function, so only two cond-lines
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			(else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
		)
	)
)

(define z1 '(+ 1 3) )
(define z2 '(+ (x 3 6) (^ 3 2)) )

; (displayln (value z1)) 				; 4
; (displayln (value z2)) 				; 27


;; original from page 135, introduced in chapter 3
(define multirember
	(lambda (a lat)
		(cond
			((null? lat) displayln '() )
			((eq? (car lat) a) (multirember a (cdr lat)))
			(else (cons (car lat) (multirember a (cdr lat))))
		)
	)
)

; (displayln (multirember "cup" '("coffee" "cup" "tea" "cup" "hick" "cup") )) 		; (coffee tea hick)


(define l5 '("shrimp" "salad" "tuna" "salad" "and" "tuna") )
; (displayln l5)
; (displayln (multirember a2 l5))


;; FUNCTION 18
(define multirember-f
	(lambda (test?)
		(lambda (a lat)
			(cond
				((null? lat) '() )
				((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
				(else (cons (car lat) ((multirember-f test?) a (cdr lat))))
			)
		)
	)
)

(define t2 eq?)
; (displayln ((multirember-f t2) a2 l5))


;; FUNCTION 19
(define multirember-eq?
	(multirember-f eq?)
)

; (displayln (multirember-eq? a2 l5))


;; FUNCTION 20
(define eq?-tuna
	(eq?-c "tuna")
)

#|
(displayln (eq?-tuna a1))  ; f  [accepts a digit]
(displayln (eq?-tuna a2))  ; t
(displayln (eq?-tuna "hello"))  ; f
(displayln (eq?-tuna l5))  ; f [also accepts a list]
|#


;; FUNCTION 21
;; multrember-f accepts a test and returns a function
;; multiremberT accepts a function and lat, and returns the result of the function on the lat
(define multiremberT
	(lambda (test? lat)
		(cond
			((null? lat) '() )
			((test? (car lat)) (multiremberT test? (cdr lat)))
			(else (cons (car lat) (multiremberT test? (cdr lat))))
		)
	)
)

; (displayln (multiremberT eq?-tuna l5))  ; (shrimp salad salad and)


;; FUNCTION 22
;; col stands for a collector, also called a continuation (page 138)
; a is item to remove
; lat is list to remove it from
; col is the function that will handle the result
; chat description of what it does:
;   - traverse list lat
;   - remove all occurrences of a from lat
;   - collect all removed items into list 'seen'
;   - passes both lists (lat and seen) to collector col
;   - col then decides what to do with the lists, eg return them, print them, compare them
; accumulator/collector passing style: each recursive call gets continuation (lambda) that says how to combine results
(define multirember&co
	(lambda (a lat col)
		(cond
			((null? lat) (col '() '() ))
			((eq? (car lat) a)
				(multirember&co a (cdr lat) 
				(lambda (newlat seen)
					(col newlat (cons (car lat) seen))
				)
			))
			(else (multirember&co a (cdr lat) 
				(lambda (newlat seen) 
					(col (cons (car lat) newlat) seen)
				)
			))
		)
	)
)


;; FUNCTION 23
;; accepts two arguments, returns whether the second is an empty list, ignores the first
; chat answer about why it has two args, when it does not use the first: so all collector functions have the same signature, making them interchangeable when building on future examples
(define a-friend
	(lambda (x y)
		(null? y)
	)
)

; 23a
(define l6 '("strawberries" "tuna" "and" "swordfish") )  ; 'lat' on page 138
; (displayln (multirember&co a2 l6 a-friend))  ; #f

; 23b
; (displayln (multirember&co a2 '() a-friend))  ; #t, because a-friend immediately used in first answer on two empty lists

; 23c
; (displayln (multirember&co a2 '("tuna") a-friend))  ; #f


;; FUNCTION 24
; col is not meant to be a parameter, it is a free variable bound in the caller's environment, not inside the function
; new-friend1 is NOT a fully-defined function by itself, it is a closure template that only makes sense inside another function whre col is already defined
; note on page 138 it describes calling multirember&co with col as 'a-friend', but in that function a-friend is passed as an arg, and I didn't assign it to a separate variable

(define col24 a-friend) ; a-friend returns whether its second arg (a list) is empty
(define lat24 '(tuna) ) ; must be a list, because car called on it

(define new-friend1
	(lambda (newlat seen)
		(col24 newlat (cons (car lat24) seen))
	)
)

; empty list passed as arg 'seen'
; (displayln (new-friend1 lat24 '() ))  ; #f, because a-friend checks if the list is empty, and it's not empty


;; FUNCTION 25
; with 'tuna' hardcoded into definition, rather than coming from car on list
(define new-friend2
  (lambda (newlat seen)
    (col24 newlat
      (cons "tuna" seen )
    )
  )
)

; (displayln (new-friend2 lat24 '() ))

;; FUNCTION 26
; putting 'a-friend' in function, rather defined outside of it
(define new-friend3
  (lambda (newlat seen)
    (a-friend newlat
      (cons "tuna" seen)
    )
  )
)

; 26a
; (displayln (new-friend3 lat24 '() )) ; #f, because list is not empty

; 26b
; (displayln (multirember&co "tuna" '("and" "tuna") a-friend)) ; #f, because "tuna" and the list ("and" "tuna") after tuna is removed from it are NOT equal

;; variation to print the lists before returning the comparison
(define a-friend-print
  (lambda (x y)
    (begin
      (displayln x)   ; print first argument
      (displayln y)   ; print second argument
      (null? y))))    ; return original result

; this is the evaluation to 'false' that is described at the top of 140, where it describes that by the time it reaches a-friend, ls1 is (and) ls2 is (tuna)
; I got extremely turned around thinking those were the inputs to multirember, which returns #t
; (displayln (multirember&co "tuna" '("and" "tuna") a-friend-print)) ; (and) (tuna) #f

; check the col expression by itself, as described on 140
; (displayln (a-friend '("and") '("tuna"))) ; #f, because second arg is not empty
; (displayln (a-friend-print '("and") '("tuna"))) ; (and) (tuna) #f, because second arg is not empty, matches book

;; FUNCTION 27
; CAUTION: this is described next to 26b at bottom of 139 but is not used either in that evaluation or the next one on 140
(define latest-friend
  (lambda (newlat seen)
  	(a-friend (cons "and" newlat) seen)
  )
)

; 27a
; is there a change calling latest-friend instead of a-friend?
; (displayln "27a")
; (displayln (multirember&co "tuna" '("and" "tuna") latest-friend)) ; #f, same as with a-friend

;; FUNCTION 28
(define last-friend
	(lambda (x y )
		(length x)
	)
)

; (displayln (multirember&co "tuna" '("strawberries" "tuna" "and" "swordfish") last-friend)) ; 3

;; FUNCTION 28
;; multiinsertLR inserts new to the left of oldLand to the right of oldR if oldL and oldR are different, a way of combining multiinsertR and multiinsertL
; comparison signature of multiinsertL: lambda (new old lat)
(define  multiinsertLR
	(lambda (new oldL oldR lat)
 		(cond
 			((null? lat) '() )
 			((eq? (car lat) oldL)
 				(cons new
 					(cons oldL (multiinsertLR new oldL oldR (cdr lat))
 					)
 				)
 			)
 			((eq? (car lat) oldR)
 				(cons oldR
 					(cons new (multiinsertLR new oldL oldR (cdr lat)))
 				)
 			)
 			(else
 				(cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))
 			)
 		)
	)
)

; test with single occurrence of oldL and oldR -> (and tuna salad shrimp and sandwich)
; (displayln (multiinsertLR 'and 'tuna 'shrimp '(tuna salad shrimp sandwich)))

; multiple occurrences of oldL and oldR -> (and tuna soup and tuna salad shrimp and shrimp and)
; (displayln (multiinsertLR 'and 'tuna 'shrimp '(tuna soup tuna salad shrimp shrimp)))

; empty list -> ()
; (displayln (multiinsertLR 'and 'tuna 'shrimp '()))


;; FUNCTION 29
;; multiinsertLR&co: additional arg that is a collector function, run on the resulting lat
;; how to define call for the initial call to the function: base case collector function, which returns the values passed to it as the result
; base collector: (lambda (newlat L R) (list newlat L R))

(define  multiinsertLR&co
	(lambda (new oldL oldR lat col)
 		(cond
 			((null? lat) (col '() 0 0)) ; zero occurrences of oldL and oldR
 			((eq? (car lat) oldL)
 				(multiinsertLR&co new oldL oldR (cdr lat)
 					(lambda (newlat L R)
 						(col (cons new (cons oldL newlat)) (add1 L) R)
 					)
 				)
 			)
 			((eq? (car lat) oldR)
 				(multiinsertLR&co new oldL oldR (cdr lat)
 					(lambda (newlat L R)
 						(col (cons oldR (cons new newlat)) L (add1 R))
 					)
 				)
 			)
 			(else
 				(multiinsertLR&co new oldL oldR (cdr lat)
 					(lambda (newlat L R)
 						(col (cons (car lat) newlat) L R)
 					)
 				)
 			)
 		)
	)
)

; expect -> (chips salty and salty fish or salty fish and chips salty) 2 2
(displayln (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) 
	(lambda (newlat L R) (list newlat L R)))
)

;; FUNCTION 30
; *-functions: work on lists that are either empty, an atom cons-ed onto a list, or a list cons-ed onto a list

(define even?
	(lambda (n)
		(= (* (quotient n 2) 2) n)
	)
)

(define evens-only*
	(lambda (l)
		(cond
			((null? l) '() )
			((atom? (car l))
				(cond
					((even? (car l))
						(cons (car l)
							(evens-only* (cdr l))
						)
					)
					(else
						(evens-only* (cdr l))
					)
				)
			)
			(else
				(cons (evens-only* (car l)) (evens-only* (cdr l)))
			)
		)
	)
)

(define l '((9 1 2 8) 3 10 ((9 9) 7 6) 2) )

(displayln (evens-only* l)) ; ((2 8) 10 (() 6) 2)

;; FUNCTION 31
;; collects the list without odd numbers, product of the even numbers, and sum of odd numbers
;; if input list is empty, returns empty list, 1, 0
(define evens-only*&co
	(lambda (l col)
		(cond
			((null? l) (col '() 1 0))
			((atom? (car l))
				(cond
					((even? (car l))
						(evens-only*&co (cdr l)
							(lambda (newl p s)
								(col (cons (car l) newl) (* (car l) p) s)
							)
						)
					)
					(else 
						(evens-only*&co (cdr l)
							(lambda (newl p s) (col newl p (addition (car l) s)))
						)
					)
				)
			)
			(else
				(evens-only*&co (car l)
					(lambda (al ap as)
						(evens-only*&co (cdr l)
							(lambda (dl dp ds)
								(col (cons al dl) (* ap dp) (addition as ds))
							)
						)
					)
				)
			)
		)
	)
)

(define the-last-friend
	(lambda (newl product sum)
		(cons sum (cons product newl))
	)
)

(displayln (evens-only*&co l the-last-friend)) ; (38 1920 (2 8) 10 (() 6) 2)