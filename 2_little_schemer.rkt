#lang racket

; Chapter 2. Do it, do it again, and again, and again...

; chatgpt version of 'lat?' (list of atoms)
;(define (lat? lst)
;	(cond
;		[(empty? lst) #t] 		; empty list considered list of atoms
;		[(atom? (first lst)) (lat? (rest lst))]  ; if first element is atom, then check the rest
;		[else #f] 								 ; if it's not an atom, return false
;	)
;)

#|
; from chatgpt, book does not define it
; define the atom? function
(define (atom? x)
	(not (list? x)))

; book p 16 version of 'lat?' (but book does NOT provide an 'atom?' function)
(define lat?
	(lambda (l)
		(cond
			((null? l) #t)
			((atom? (car l)) (lat? (cdr l)))
			(else #f))))

(displayln (lat? '(1 2 3 4)))
(define l '("Jack" "Sprat" "could" "eat" "no" "chicken" "fat"))
(displayln (lat? l))
(define l-2 '(("Jack" "Sprat") "could" "eat" "no" "chicken" "fat"))
(displayln (lat? l-2))
|#

; book p22
(define member?
	(lambda (a lat)
		(cond
			((null? lat) #f)
			(else (or (eq? (car lat) a) (member? a (cdr lat)))))))
		; if lat is empty:return true
		; else lat NOT empty: check whether car equals a OR if a is in cdr of lat
; because of lazy evaluation: if car lat is true, OR statement is true, so return
;     if car lat is false, OR statement depends on evaluating cdr lat, so continues (recursive case)

(displayln (member? "meat" '("mashed" "potatoes" "and" "meat" "gravy")))
(displayln (member? "meat" '("mashed" "potatoes" "and" "gravy")))