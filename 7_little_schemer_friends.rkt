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

(displayln (intersect set-6 set-7))  ; (chicken wings 5)
(displayln (intersect set-7 set-8))  ; (chicken light)
(displayln (intersect set-8 set-7))  ; (light chicken)  [returned order is from set1]

