#lang racket

;;; Chapter 7. Friends and Relations

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

; p22
(define member?
	(lambda (a lat)
		(cond
			((null? lat) #f)
			(else (or (eq? (car lat) a) (member? a (cdr lat))))
		)
	)
)


(define multirember
	(lambda (a lat)
		(cond
			((null? lat) displayln '() )
			(else
				(cond
					((eq? (car lat) a) (multirember a (cdr lat)))
					(else (cons (car lat) (multirember a (cdr lat))))
				)
			)
		)
	)
)


(define (atom? x)
	(not (list? x)))(chips and fish fried or fish and fried)


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


(define (firsts lst-of-lists)
  (map car lst-of-lists))


;; cadr is shorthand for (car (cdr x)), which retrieves the second element of a list
(define (seconds lst-of-lists)
  (map cadr lst-of-lists))


;; seconds without using 'cadr'
; (define (seconds lst-of-lists)
;   (map (lambda (lst) (car (cdr lst))) lst-of-lists))


;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; FUNCTION 1
;; returns true if no atom appears more than once
(define set?
	(lambda (lat)
		(cond
			((null? lat) #t)
			((member? (car lat) (cdr lat)) #f)
			(else (set? (cdr lat)))
		)
	)
)

(define lat-1 '("apples" "peaches" "pears" "plums") )
(define lat-2 '() )
(define lat-3 '("apples" 3 "pear" 4 9 "apple") )
(define lat-4 '("apples" 3 "pear" 4 9 3) )

#|
(displayln (set? lat-1)) 				; #t
(displayln (set? lat-2)) 				; #t
(displayln (set? lat-3)) 				; #t
(displayln (set? lat-4)) 				; #f



;; FUNCTION 2
;; makeset using member?
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '() )
			((member? (car lat) (cdr lat)) (makeset (cdr lat)))
			(else (cons (car lat) (makeset (cdr lat))))
		)
	)
)

(define lat-5 '("apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach") )
(displayln (makeset lat-5))  					;  (pear plum apple lemon peach)    [book has order different: (apple, peach, pear, plum, lemon)]


;; FUNCTION 3
;; makeset using multirember
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '() )
			(else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
		)
	)
)

(define lat-6 '("apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach") )
; (displayln (makeset lat-6)) 				; (apple peach pear plum lemon) 			[this implementation returns different order]
|#

;; FUNCTION 4
(define subset-1?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			(else 
				(cond 
					((member? (car set1) set2) (subset-1? (cdr set1) set2))
					(else #f)
				)
			)
		)
	)
)


(define set1 '(5 "chicken" "wings") )
(define set2 '(5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings") )

; (displayln (subset-1? set1 set2))  ; #t
; (displayln (subset-1? set2 set1))  ; #f


;; FUNCTION 5
;; shorter subset?
(define subset-2?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			((member? (car set1) set2) (subset-2? (cdr set1) set2))
			(else #f)
		)
	)
)

; (displayln (subset-2? set1 set2))  ; #t
; (displayln (subset-2? set2 set1))  ; #f


;; FUNCTION 6
;; subset with and
(define subset-3?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			(else (and (member? (car set1) set2) (subset-3? (cdr set1) set2)))
		)
	)
)

; (displayln (subset-3? set1 set2))  ; #t
; (displayln (subset-3? set2 set1))  ; #f

;; FUNCTION 7
(define eqset-1?
	(lambda (set1 set2)
		(cond
			((subset-2? set1 set2) (subset-2? set2 set1))
			(else #f)
		)
	)
)

(define set-1 '(5 "chicken" "wings") )
(define set-2 '(5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings") )
(define set-3 '("chicken" "wings" 5) )

; (displayln (eqset-1? set-1 set-2))  ; #f
; (displayln (eqset-1? set-1 set-3))  ; #t   [sets are order independent]


;; FUNCTION 8
;; eqset with only one cond line
(define eqset-2?
	(lambda (set1 set2)
		(cond
			(else (and (subset-2? set1 set2) (subset-2? set2 set1)))
		)
	)
)

; (displayln (eqset-2? set-1 set-2))  ; #f
; (displayln (eqset-2? set-1 set-3))  ; #t


;; FUNCTION 9
;; eqset one-liner
(define eqset-3?
	(lambda (set1 set2)
		(and (subset-2? set1 set2) (subset-2? set2 set1))
	)
)

; (displayln (eqset-3? set-1 set-2))  ; #f
; (displayln (eqset-3? set-1 set-3))  ; #t

;; FUNCTION 10
;; returns true or false for whether there is at least one atom in both sets
(define intersect-1?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			((member? (car set1) set2) #t)
			(else (intersect-1? (cdr set1) set2))
		)
	)
)

(define set-4 '("bird") )
(define set-5 '("bird" "wings") )

; (displayln (intersect-1? set-3 set-4))  ; #f
; (displayln (intersect-1? set-3 set-5))  ; #t

;; FUNCTION 11
;; intersect with or
(define intersect-2?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			(else (or (member? (car set1) set2) (intersect-2? (cdr set1) set2)))
		)
	)
)

; (displayln (intersect-2? set-3 '() ))   ; #f
; (displayln (intersect-2? set-3 set-4))  ; #f
; (displayln (intersect-2? set-3 set-5))  ; #t

#|
Compare subset? and intersect? (page 115)
	- if a set is null, subset returns true but intersect returns false
	- at the member check, if true intersect returns, but subset has to continue with the list
	- unless set1 is null, subset has to iterate on every item in set1

; returns true if at least one atom is in both sets
(define intersect-1?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			((member? (car set1) set2) #t)
			(else (intersect-1? (cdr set1) set2))
		)
	)
)

; returns true if every item in set1 is also in set2
(define subset-2?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			((member? (car set1) set2) (subset-2? (cdr set1) set2))
			(else #f)
		)
	)
)
|#


;; FUNCTION 12
(define intersect
	(lambda (set1 set2)
		(cond
			((null? set1) '() )
			((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
			(else (intersect (cdr set1) set2))
		)
	)
)

(define set-6 '("chicken" "wings" 5) )
(define set-7 '(5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings") )
(define set-8 '("light" "piece" "chicken") )

; (displayln (intersect set-6 set-7))  ; (chicken wings 5)
; (displayln (intersect set-7 set-8))  ; (chicken light)
; (displayln (intersect set-8 set-7))  ; (light chicken)  [returned order is from set1]


;; FUNCTION 13
(define union
	(lambda (set1 set2)
		(cond
			((null? set1) set2)
			((member? (car set1) set2) (union (cdr set1) set2))
			(else (cons (car set1) (union (cdr set1) set2)))
		)
	)
)

(define set-9 '("stewed" "tomatoes" "and" "macaroni" "casserole") )
(define set-10 '("macaroni" "and" "cheese") )

; (displayln (union set-9 set-10))  ; (stewed tomatoes casserole macaroni and cheese)


;; FUNCTION 14
(define setdiff
	(lambda (set1 set2)
		(cond
			((null? set1) '() )
			((member? (car set1) set2) (setdiff (cdr set1) set2))
			(else (cons (car set1) (setdiff (cdr set1) set2)))
		)
	)
)

; (displayln (setdiff set-9 set-10))  ; (stewed tomatoes casserole)


;; FUNCTION 15
(define intersectall
	(lambda (l-set)
		(cond
			((null? (cdr l-set)) (car l-set))
			(else (intersect (car l-set) (intersectall (cdr l-set))))
		)
	)
)

#|
(define l-set '((6 "pears" "and")
								(3 "peaches" "and" 6 "peppers")
								(8 "pears" "and" 6 "plums")
								("and" 6 "prunes" "with" "some" "apples")))

(displayln (intersectall l-set))    ; (6 and)
|#

; this chatgpt answer used function 'list' not covered by the book
; (define l-set (list set-9 set-10))

; way based on page 119 function 'build' that does not use 'list'
; use cons to add all the sets to an empty list
; book has not used a type to enforce or define sets
; but lists and sets are immutable, so need to make a new one
(define l-set (cons set-9 (cons set-10 '() )) )

; (displayln l-set)
; (displayln (intersectall l-set))    ; (and macaroni)


;; FUNCTION 16
;; returns true or false for whether x is a list with two s-expressions
(define a-pair?
	(lambda (x)
		(cond
			((atom? x) #f)
			((null? x) #f)
			((null? (cdr x)) #f)
			((null? (cdr (cdr x))) #t)
			(else #f)
		)
	)
)


(define l-1 '("pear" "pear"))
; (displayln (a-pair? l-1)) 			; #t

(define l-2 '(3 7) )
; (displayln (a-pair? l-2)) 			; #t

(define l-3 '((2) ("pair")) )
; (displayln (a-pair? l-3)) 			; #t

(define l-4 '((2) ("pair") 3) )
; (displayln (a-pair? l-4)) 			; #f


;; FUNCTION 17
;; used to make representation of pairs
(define first
	(lambda (p)
		(car p)
	)
)

;; FUNCTION 18
;; used to make representation of pairs
(define second
	(lambda (p)
		(car (cdr p))
	)
)

;; FUNCTION 19
;; used to make representation of pairs
;; does not enforce a type check that args are sets
(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '() ))     ; empty list used to initialize and empty list, for set2 to be added onto: lists are immutable, so need new list
	)
)

;; FUNCTION 20
;; used to make representation of pairs
(define third
	(lambda (l)
		(car (cdr (cdr l)))
	)
)

; (print-type l-2) 					; (3 7) - List   [also defined by book as a pair, but not as a type in Scheme]
; (displayln (first l-2))  	; 3
; (displayln (second l-2))	; 7
(define l-5 (build l-2 l-3))
; (print-type l-5)  					; ((3 7) ((2) (pair))) - List


;; FUNCTION 21
;; returns boolean for whether the input is a finite function
;; finite function: a list of pairs in which no first element of any pair is the same as any other first element
(define fun?
	(lambda (rel)
		(set? (firsts rel))
	)
)

(define rel-1 '((4 3) (4 2) (7 6) (6 2) (3 4)) )
; (displayln rel-1)
; (displayln (fun? rel-1))  ; #f

(define rel-2 '((4 3) (7 6) (6 2) (3 4)) )
; (displayln rel-2)
; (displayln (fun? rel-2))  ; #t


;; FUNCTION 22
;; reverses each pair
(define revrel-1
	(lambda (rel)
		(cond
			((null? rel) '() )
			(else (cons 
				(build (second (car rel)) (first (car rel))) 
				(revrel-1 (cdr rel))
			))
		)
	)
)

(define rel-3 '((8 "a") ("pumpkin" "pie") ("piece" "of")) )
; (displayln (revrel-1 rel-3))  ; ((a 8) (pie pumpkin) (of piece))


;; FUNCTION 23
;; revrel without the helper functions
(define revrel-2
	(lambda (rel)
		(cond
			((null? rel) '() )
			(else 
				(cons 
					(cons (car (cdr (car rel))) (cons (car (car rel)) '() ))
					(revrel-2 (cdr rel))
				)
			)
		)
	)
)

; (displayln (revrel-2 rel-3))  ; ((a 8) (pie pumpkin) (of piece))


;; FUNCTION 24
;; reverses the two components of a pair [unlike a set, a pair has defined order]
(define revpair
	(lambda (pair)
		(build (second pair) (first pair))
	)
)

;; FUNCTION 25
;; revrel using the pair-reverse helper function
(define revrel-3
	(lambda (rel)
		(cond
			((null? rel) '() )
			(else (cons (revpair (car rel)) (revrel-3 (cdr rel))))
		)
	)
)

; (displayln (revrel-3 rel-3))  ; ((a 8) (pie pumpkin) (of piece))


;; FUNCTION 26
;; returns true or false for whether the second items in a relation are also a set
(define fullfun?
	(lambda (fun)
		(set? (seconds fun))
	)
)

(define rel-4 '(("grape" "raisin") ("plum" "prune") ("stewed" "grape")) )
; (displayln (seconds rel-4))   ; (raisin prune grape)
; (displayln (fullfun? rel-4))  ; #t

(define rel-5 '(("grape" "raisin") ("plum" "raisin") ("stewed" "grape")) )
; (displayln (seconds rel-5))   ; (raisin raisin grape)
; (displayln (fullfun? rel-5))  ; #f


;; FUNCTION 27
;; another name and way to write 'fullfun'
;; this captures how every value (that goes with a key) is unique
(define one-to-one?
	(lambda (fun)
		(fun? (revrel-3 fun))
	)
)

(displayln (one-to-one? rel-4))  ; #t
(displayln (one-to-one? rel-5))  ; #f