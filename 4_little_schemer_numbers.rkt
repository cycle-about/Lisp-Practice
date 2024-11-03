#lang racket

; Chapter 4. Number Games


(define addition
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (add1 (addition n (sub1 m))))
		)
	)
)

; (displayln (addition 3 4))

(define subtraction
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (sub1 (subtraction n (sub1 m))))
		)
	)
)

; (displayln (subtraction 7 4))

(define addtup
	(lambda (tup)
		(cond
			((null? tup) 0)
			(else (addition (car tup) (addtup (cdr tup))))
		)
	)
)

; (displayln (addtup '(2 3 4) ))

(define multiply
	(lambda (n m)
		(cond
			((zero? m) 0)
			(else (addition n (multiply n (sub1 m))))
		)
	)
)

; (displayln (multiply 5 2))

(define addtups1
	(lambda (tup1 tup2)
		(cond 
			((and (null? tup1) (null? tup2)) displayln '() )
			(else (cons (addition (car tup1) (car tup2)) (addtups1 (cdr tup1) (cdr tup2))))
		)
	)
)

; (displayln (addtups1 '(2 4 6) '(3 5 7) ))
; error message if try to input tuples of different lengths
;     "car: contract violation. expected: pair? given: '()"

; version that handles tuples of different lengths
(define addtups2
	(lambda (tup1 tup2)
		(cond
			((null? tup1) tup2)
			((null? tup2) tup1)
			(else (cons (addition (car tup1) (car tup2)) (addtups2 (cdr tup1) (cdr tup2))))
		)
	)
)

; (displayln (addtups2 '(2 4 6) '(3 5 7 1 2) ))

(define greater
	(lambda (n m)
		(cond
			((zero? m) #t)
			((zero? n) #f)
			(else (greater (sub1 n) (sub1 m)))
		)
	)
)

; (displayln (greater 7 3))
; (displayln (greater 2 9))

(define lesser
	(lambda (n m)
		(cond
			((zero? m) #f)
			((zero? n) #t)
			(else (lesser (sub1 n) (sub1 m)))
		)
	)
)

; (displayln (lesser 1 9))
; (displayln (lesser 8 3))

(define equals
	(lambda (m n)
		(cond
			((zero? m) (zero? n))
			((zero? n) #f)
			(else (equals (sub1 n) (sub1 m)))
		)
	)
)

; (displayln (equals 5 5))
; (displayln (equals 3 7))
; (displayln (equals 0 0))  ; #t
; (displayln (equals -1 -1))  ; command hangs

(define exponent
	(lambda (n m)
		(cond
			((zero? m) 1)
			(else (multiply n (exponent n (sub1 m))))
		)
	)
)

; (displayln (exponent 2 4))

(define division
	(lambda (n m)
		(cond
			((lesser n m) 0)
			(else (add1 (division (subtraction n m) m)))
		)
	)
)

; (displayln (division 15 4))

(define length
	(lambda (lat)
		(cond
			((null? lat) 0)
			(else (add1 (length (cdr lat))))
		)
	)
)

; (displayln (length '("hello") ))  ; 1
; (displayln (length '(4 5 7) )) ; 3

; return the item at index n
; scheme is 1-indexed, index 0 is an error
(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(else (pick (sub1 n) (cdr lat)))
		)
	)
)

; (displayln (pick 2 '(0 1 2 3 4) ))  ; 1
; (displayln (pick 0 '(0 1 2 3 4) ))  ; cdr: contract violation, expected: pair?, given: '()

; return the list with item at index n removed
(define rempick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (cdr lat))
			(else (cons (car lat) (rempick (sub1 n) (cdr lat))))
		)
	)
)

; (displayln (rempick 2 '(0 1 2 3 4) ))  ; (0 2 3 4)
; (displayln (rempick 1 '(0 1 2 3 4) ))  ; (1 2 3 4)

; removes all numbers from the list
(define no-nums
	(lambda (lat)
		(cond
			((null? lat) (displayln '("list is null") ))
			; ((null? lat) ())  														; "missing procedure expression", can't pass an actual empty expression for the 'do nothing' case
			(else (cond
				((number? (car lat)) (no-nums (cdr lat)))
				(else (cons (car lat) (no-nums (cdr lat))))
			))
		)
	)
)

; (displayln (no-nums '("hello" 3 "world" 7 "!" 4 "6") ))  			; (list is null) (hello world ! 6 . #<void>)
; (displayln (no-nums '("hello" "world") )) 										; (list is null) (hello world . #<void>)
; example inputs from book
; (displayln (no-nums '(5 "pears" 6 "prunes" 9 "dates") )) 				; still hits empty list
; (displayln (no-nums '(5 6 9 ) )) 																; only numbers: still hits empty list
; (displayln (no-nums '(5 6 9 "hi" "bye") )) 											; has string but ends with number: still hits empty list
; ???? this seems to always hit the empty list, unlike other example functions


(define all-nums
	(lambda (lat)
		(cond
			((null? lat) (displayln '() ))
			(else
				(cond
					((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
					(else (all-nums (cdr lat)))
				)
			)
		)
	)
)

; (displayln (all-nums '("hello" 3 "world" 7 "!" 4 "6") ))  			; () (3 7 4 . #<void>)
; (displayln (all-nums '("hello" "world") )) 											; () #<void>
; ???? this one also always hits the empty list clause


; returns true or false depending on whether two atoms are the sam
(define eqan?
	(lambda (a1 a2)
		(cond
			((and (number? a1) (number? a2)) (= a1 a2))  			; if a1 and a2 are numbers: return result of numeric equality check
			((or (number? a1) (number? a2)) #f) 							; if only one of them is a number: return false
			(else (eq? a1 a2)) 																; if neither of those: return result of non-numeric equality check
		)
	)
)

; (displayln "running eqan?")
; (displayln (eqan? 3 4))    	; #f
; (displayln (eqan? "hi" 3))  ; #f
; (displayln (eqan? 4 4)) 		; #t
; (displayln (eqan? #f #f)) 	; #t

; counts the number of times atom appears in lat
(define occur
	(lambda (a lat)
		(cond
			((null? lat) 0)
			(else
				(cond
					((eq? (car lat) a) (add1 (occur a (cdr lat))))
					(else (occur a (cdr lat)))
				)
			)
		)
	)
)

(displayln (occur "hi" '("hi" "bye" 3 "hi" 1) )) 	; 2
(displayln (occur 3 '("hi" "bye" 3 "hi" 1) )) 		; 1
(displayln (occur "" '("" "hi" 3) )) 							; 1

