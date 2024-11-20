#lang racket

;;; Chapter 5. Full of Stars
(displayln "chapter 5")

; from chatgpt, book does not define it
; define the atom? function 
(define (atom? x)
	(not (list? x)))

;; function 1
(define rember*
	(lambda (a l)
		(cond  																; start cond_1
			((null? l) '() ) 										; start cond_1_1_if
			((atom? (car l)) 										; cond_1_2_if: if car l is an atom, answer the next question
				(cond 																				; start cond_2
					((eq? (car l) a) (rember* a (cdr l))) 			; cond_2_1_if
					(else (cons (car l) (rember* a (cdr l))))		; cond_2_2_else
				) 																						; end cond_2
			)																		; end cond_1_2_if
			(else (cons (rember* a (car l)) (rember* a (cdr l))))  ; cond_1_3_else
		) 	; end cond_1
	)			; end lambda
) 			; end define

(define l_1 '((("tomato" "sauce")) (("bean") "sauce") ("and" ("flying")) "sauce"))
; (printf "before: ~a\n" l_1)
; (printf "after: ~a\n\n" (rember* "sauce" l_1))


;; function 2
(define insertR*
	(lambda (new old l)
		(cond 								; start cond_1
			((null? l) '() ) 		; cond_1_1_if
			((atom? (car l)) 		; start cond_1_2_if
				(cond 									 																									; start cond_2
					((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))) 		; cond_2_1_if
					(else (cons (car l) (insertR* new old (cdr l)))) 												; cond_2_2_else
				) 																																					; end cond_2
			)  									; end cond_1_2_if
			(else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))) 	; cond_1_3_else
		) 										; end cond_1
	)  	; end lambda
)  		; end define

(define l_2 '(("how" "much" ("wood")) "could" (("a" ("wood") "chuck")) ((("chuck"))) ("if" ("a") (("wood" "chuck"))) "could" "chuck" "wood" ))
; (displayln l_2)
; (displayln (insertR* "roast" "chuck" l_2))


(define addition
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (add1 (addition n (sub1 m))))
		)
	)
)

;; function 3
(define occur*
	(lambda (a l)
		(cond   							; start cond_1
			((null? l) 0) 			; cond_1_1_if
			((atom? (car l))		; start cond_1_2_if
				(cond 																					; start cond_2
					((eq? (car l) a) (add1 (occur* a (cdr l)))) 	; cond_2_1_if
					(else (occur* a (cdr l))) 										; cond_2_2_else
				) 																							; end cond_2
			) 									; end cond_1_2_if
			(else (addition (occur* a (car l)) (occur* a (cdr l))))  	; cond_1_3_else
		)
	)	; end lambda
)  	; end define

(define l_3 '(("banana") ("split" (((("banana" "ice"))) ("cream" ("banana")) "sherbet" )) ("banana") ("bread") ("banana" "brandy")))
; if define the lat without words in quotes, it runs but returns a count of 0
; (displayln (occur* "banana" l_3))


;; function 4
(define subst*
	(lambda (new old l)
		(cond
			((null? l) '() )
			((atom? (car l))
				(cond
					((eq? (car l) old) (cons new (subst* new old (cdr l))))
					(else (cons (car l) (subst* new old (cdr l))))
				)
			)
			(else (cons (subst* new old (car l)) (subst* new old (cdr l))))
		)
	)
)

; (displayln (subst* "orange" "banana" l_3))


;; function 5
(define insertL*
	(lambda (new old l)
		(cond
			((null? l) '() )
			((atom? (car l))  ; start cond_1_2_if
				(cond
					((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
					(else (cons (car l) (insertL* new old (cdr l))))
				)
			)  ; end cond_1_2_if
			(else (cons (insertL* new old (car l)) (insertL* new old (cdr l)) ))
		)
	)
)

; (displayln l_2)
; (displayln (insertL* "pecker" "chuck" l_2))


;; function 6
(define member*
	(lambda (a l)
		(cond
			((null? l) #f) 																								; return false
			((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))) )  	; return that true/false evaluation
			(else (or (member* a (car l)) (member* a (cdr l)))) 				  ; return that true/false evaluation
		)
	)
)

(define l_4 '(("potato") ("chips" (("with") "fish") ("chips"))) )
; (displayln (member* "chips" l_4))


;; function 7
; finds the leftmost atom in a non-empty list of S-expressions that does not contain the empty list
(define leftmost
	(lambda (l)
		(cond
			((atom? (car l)) (car l))
			(else (leftmost (car l)))
		)
	)
)

; (displayln (leftmost l_4))  ; potato
; (displayln (leftmost l_2))  ; how
; (displayln (leftmost l_1))  ; tomato


; returns true or false for whether two atoms are the sam
(define eqan?
	(lambda (a1 a2)
		(cond
			((and (number? a1) (number? a2)) (= a1 a2))  			; if a1 and a2 are numbers: return result of numeric equality check
			((or (number? a1) (number? a2)) #f) 							; if only one of them is a number: return false
			(else (eq? a1 a2)) 																; if neither of those: return result of non-numeric equality check
		)
	)
)


;; function 8
;; returns true or false for whether two lists are the same
(define eqlist_1?
	(lambda (l1 l2)
		(cond
			((and (null? l1) (null? l2)) #t)
			((and (null? l1) (atom? (car l2))) #f)  ; reaching this question means l2 is not null, otherwise first question would have been true
			((null? l1) #f) 												; reaching here means that if l1 is null, l2 is both not null and its first item is a list
			((and (atom? (car l1)) (null? l2)) #f)
			((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist_1? (cdr l1) (cdr l2))))
			((atom? (car l1)) #f)
			((null? l2) #f)
			((atom? (car l2)) #f)
			(else
				(and (eqlist_1? (car l1) (car l2)) (eqlist_1? (cdr l1) (cdr l2)))
			)
		)
	)
)

(define l_5 '("strawberry" "ice" "cream") )
(define l_6 '("strawberry" "cream" "ice") )
(define l_7 '(("banana") (("split"))) )
(define l_8 '("banana" ("split")) )
(define l_9 '("beef" (("sausage")) ("and" ("soda"))) )
(define l_10 '("beef" (("salami")) ("and" ("soda"))) )

#|
(displayln (eqlist_1? l_5 l_6))   ; #f
(displayln (eqlist_1? l_5 l_5))   ; #t
(displayln (eqlist_1? l_7 l_8))   ; #f
(displayln (eqlist_1? l_9 l_10))  ; #f
(displayln (eqlist_1? l_9 l_9))   ; #t
|#


; function 9
(define eqlist_2?
	(lambda (l1 l2)
		(cond
			((and (null? l1) (null? l2)) #t)
			((or (null? l1) (null? l2)) #f)
			((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist_2? (cdr l1) (cdr l2))))
			((or (atom? (car l1)) (atom? (car l2))) #f)
			(else
				(and (eqlist_2? (car l1) (car l2)) (eqlist_2? (cdr l1) (cdr l2)))
			)
		)
	)
)

#|
(displayln (eqlist_2? l_5 l_6))   ; #f
(displayln (eqlist_2? l_5 l_5))   ; #t
(displayln (eqlist_2? l_7 l_8))   ; #f
(displayln (eqlist_2? l_9 l_10))  ; #f
(displayln (eqlist_2? l_9 l_9))   ; #t
|#


; function 10
; returns whether two s-expressions are equal
(define equal_1?
	(lambda (s1 s2)
		(cond
			((and (atom? s1) (atom? s2)) (eqan? s1 s2))
			((atom? s1) #f)
			((atom? s2) #f)
			(else (eqlist_2? s1 s2))
		)
	)
)

#|
(displayln (equal_1? l_5 l_6))   ; #f
(displayln (equal_1? l_5 l_5))   ; #t
(displayln (equal_1? l_7 l_8))   ; #f
(displayln (equal_1? l_9 l_10))  ; #f
(displayln (equal_1? l_9 l_9))   ; #t
|#


; function 11
(define equal_2?
	(lambda (s1 s2)
		(cond
			((and (atom? s1) (atom? s2)) (eqan? s1 s2))
			((or (atom? s1) (atom? s2)) #f) 		; combined second and third questions from v1
			(else (eqlist_2? s1 s2))
		)
	)
)

#|
(displayln (equal_2? l_5 l_6))   ; #f
(displayln (equal_2? l_5 l_5))   ; #t
(displayln (equal_2? l_7 l_8))   ; #f
(displayln (equal_2? l_9 l_10))  ; #f
(displayln (equal_2? l_9 l_9))   ; #t
|#


; function 12
(define eqlist_3?
	(lambda (l1 l2)
		(cond
			((and (null? l1) (null? l2)) #t)
			((or (null? l1) (null? l2)) #f)
			(else (and (equal_2? (car l1) (car l2)) (eqlist_3? (cdr l1) (cdr l2))))
		)
	)
)

#|
(displayln (eqlist_3? l_5 l_6))   ; #f
(displayln (eqlist_3? l_5 l_5))   ; #t
(displayln (eqlist_3? l_7 l_8))   ; #f
(displayln (eqlist_3? l_9 l_10))  ; #f
(displayln (eqlist_3? l_9 l_9))   ; #t
|#


; function 13
; rember that takes a list of s-expressions and any s-expression (instead of lat and atom)
; this is NOT a star function because it recurs only with the sdr of l
; star functions recur down the car of the list, instead of only the cdr (page 81)
; removes the first instance only of the s-expression where it is in the first level of the list
(define rember_1
	(lambda (s l)
		(cond
			((null? l) '() )
			((atom? (car l)) 
				(cond
					((equal_2? (car l) s) (cdr l))
					(else (cons (car l) (rember_1 s (cdr l))))
				)
			)
			(else 
				(cond
					((equal_2? (car l) s) (cdr l))
					(else (cons (car l) (rember_1 s (cdr l))))
				)
			)
		)
	)
)

; (printf "before: ~a\n" l_1) 											; (((tomato sauce)) ((bean) sauce) (and (flying)) sauce)
; (printf "after: ~a\n\n" (rember_1 "sauce" l_1)) 	; (((tomato sauce)) ((bean) sauce) (and (flying)))
; (printf "after: ~a\n\n" (rember_1 "flying" l_1))    ; unchanged
; (printf "after: ~a\n\n" (rember_1 "and" l_1)) 			; unchanged

; (printf "before: ~a\n" l_2) 											; ((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)
; (printf "after: ~a\n\n" (rember_1 "chuck" l_2)) 	; ((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could wood)
; (printf "after: ~a\n\n" (rember_1 "a" l_2)) 			; unchanged

(define l_11 '("beef" (("salami")) "beef" ("and" ("soda")) "beef") )
; (printf "before: ~a\n" l_11)
; (printf "after: ~a\n\n" (rember_1 "beef" l_11)) 		; (((salami)) beef (and (soda)) beef)

(define l_12 '(("beef") (("salami")) "beef" ("and" ("soda")) "beef") )
; (printf "before: ~a\n" l_12)
; (printf "after: ~a\n\n" (rember_1 "beef" l_12)) 			; ((beef) ((salami)) (and (soda)) beef)

; remove a list, rather than an atom
; (printf "before: ~a\n" l_12)
; (printf "after: ~a\n\n" (rember_1 '("beef") l_12)) 					; (((salami)) beef (and (soda)) beef)
; (printf "after: ~a\n\n" (rember_1 '("and" ("soda")) l_12))  	; ((beef) ((salami)) beef beef)


; function 14
; simplified rember
(define rember_2
	(lambda (s l)
		(cond
			((null? l) '() )
			(else (cond
				((equal? (car l) s) (cdr l))
				(else (cons (car l) (rember_2 s (cdr l))))
			))
		)
	)
)

#|
(printf "before: ~a\n" l_12)
(printf "after: ~a\n" (rember_2 "beef" l_12)) 							; ((beef) ((salami)) (and (soda)) beef)
(printf "after: ~a\n" (rember_2 '("beef") l_12)) 						; (((salami)) beef (and (soda)) beef)
(printf "after: ~a\n" (rember_2 '("and" ("soda")) l_12))  	; ((beef) ((salami)) beef beef)
|#


; function 15
; further simplified
(define rember_3
	(lambda (s l)
		(cond
			((null? l) '() )
			((equal? (car l) s) (cdr l))  ; return the whole rest of the list if s-expression is first item in the list
			(else (cons (car l) (rember_3 s (cdr l))))
		)
	)
)

(printf "before: ~a\n" l_12)
(printf "after: ~a\n" (rember_2 "beef" l_12)) 							; ((beef) ((salami)) (and (soda)) beef)
(printf "after: ~a\n" (rember_2 '("beef") l_12)) 						; (((salami)) beef (and (soda)) beef)
(printf "after: ~a\n" (rember_2 '("and" ("soda")) l_12))  	; ((beef) ((salami)) beef beef)