#lang racket

;;; Chapter 9. ...and Again, and Again, and Again,...
;;; caution: this chapter addresses the halting problem and non-terminating functions
;;; consider that when implemeting functions or not 

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

;; from chapter 4
;; return the item at index n
;; scheme is 1-indexed, index 0 is an error
(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(else (pick (sub1 n) (cdr lat)))
		)
	)
)

;; from chapter 7
(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '() ))     ; empty list used to initialize and empty list, for set2 to be added onto: lists are immutable, so need new list
	)
)

;; from chapter 7
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

;; made new for this chapter
;; equivalent version without lambda
;;   (define (atom? x) (and (not (pair? x)) (not (null? x))))
;;   (define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))
(define atom? 
	(lambda (x) 
		(and (not (pair? x)) (not (null? x))))
)

;; copied from chapter 8
(define addition
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (add1 (addition n (sub1 m))))
		)
	)
)

;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; FUNCTION 1
;; page 150 but used by function 2
;; sorn stands for symbol or number
;; behavior:
;;   - if sorn is a number n, it looks up the nth element of lat (with pick) and recurses on that new value
;;   - if sorn is a symbol, it stops and returns whether that symbol is eq? to a
;; unnatural resursion because it does not 'consume' the list, instead jumps around within the entire list, and there is not a measure that decreases with each call
;; the recursion is not structurally decreasing, so how it behaves and whether it ever terminates depends on the pointers in it
;; natural recursion: follows shape of data and gets structurally smaller each iteration (for list recurse on cdr, for number recurse on sub1)
;; 

(define keep-looking
	(lambda (a sorn lat)
		(cond
			((number? sorn)
				(keep-looking a (pick sorn lat) lat)
			)
			(else
				(eq? sorn a)
			)
		)
	)
)

(define a1 'caviar)
(define lat1 '(6 2 4 caviar 5 7 3) ) ; don't use quote char on string when in a list

; (displayln (keep-looking a1 3 lat1 )) ; #t
; (displayln (keep-looking a1 4 lat1 )) ; #t
; my additional tests
; (displayln (keep-looking a1 1 lat1 )) ; #t
; (displayln (keep-looking 'grits 1 lat1 )) 	; #f
; (displayln (keep-looking a1 6 lat1 )) ; #t


;; FUNCTION 2
;; actually on p 149, but uses function 1
(define looking
	(lambda (a lat)
		(keep-looking a (pick 1 lat) lat)
	)
)

(define lat2 '(6 2 grits caviar 5 7 3) )

; (displayln "Function 2")
; (displayln (looking a1 lat1)) ; #t
; (displayln (looking a1 lat2)) ; #f

(define lat3 '(7 2 4 7 5 6 3) )

; (displayln (looking a1 lat3)) ; no printout, infinite loop, 7 - 3 - 4 - 7 - 3 - 4 ...

;; FUNCTION 3
;; shorter function that does not reach its goal for any of its args, since only calls itself with same input
;; this is the most partial function
(define eternity
	(lambda (x)
		(eternity x)
	)
)

; (displayln "running eternity")
; (displayln (eternity 3)) ; no output

;; FUNCTION 4
;; takes a pair whose first component is a pair, builds a pair by shifting second part of first component into second component
(define shift
	(lambda (pair)
		(build (first (first pair))
			(build (second (first pair)) (second pair))
		)
	)
)

(define x1 '((a b) (c d)) )

; (displayln (shift x1)) ; (a (b (c d)))

;; FUNCTION 5
;; like keep-looking, it uses a different arg for recursive call but change is not guaranteed to get closer to goal
;; what changes about args to align and its recursive calls: first component becomes simpler (because it is only a part of the original pair's first component), though second part becomes more complicated (because it becomes the second member of a new pair)
;; pora stands for pair-or-atom
(define align
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora)) (align (shift pora)))
			(else (build (first pora) (align (second pora))))
		)
	)
)

#| 
(displayln (align '(a b) )) 							; (a b)
(displayln (align '(((a b) c) d) )) 			; (a (b (c d)))
(displayln (align '((a b) (c d)) ))				; (a (b (c d)))
(displayln (align '((((a b) c) d) e) )) 	; (a (b (c (d e))))
|#

;; FUNCTION 6
(define length*
	(lambda (pora)
		(cond
			((atom? pora) 1)
			(else (addition (length* (first pora)) (length* (second pora))))
		)
	)
)

#| 
(displayln (length* 'a))                       ; 1
(displayln (length* '(a b)))                   ; 2
(displayln (length* '((a b) c)))               ; 3
(displayln (length* '(((a b) c) d)))           ; 4
(displayln (length* '((a (b c)) (d e))))       ; 5
(displayln (length* '((1 2) (3 4))))           ; 4
|#

;; FUNCTION 7
;; compared to length* pays more attention (twice as much) to the first component, better for determining length of arg
;; each atom counts as 1, and every time an atom lies in a left branch its contribution is doubled for that level
(define weight*
	(lambda (pora)
		(cond
			((atom? pora) 1)
			(else (addition (* (weight* (first pora)) 2) (weight* (second pora))))
		)
	)
)

(displayln (weight* 'a))                        ; 1
(displayln (weight* '(a b) ))                   ; 3
(displayln (weight* '(a (b c)) ))               ; 5
(displayln (weight* '((a b) c) ))               ; 7
(displayln (weight* '((1 2) (3 4)) ))           ; 9
(displayln (weight* '((a (b c)) (d e)) ))       ; 13
(displayln (weight* '(((a b) c) d) ))           ; 15