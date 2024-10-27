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

(displayln (equals 5 5))
(displayln (equals 3 7))
(displayln (equals 0 0))  ; #t
(displayln (equals -1 -1))  ; command hangs

