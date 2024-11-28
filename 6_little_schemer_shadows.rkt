#lang racket

;;; Chapter 6. Shadows

; arithmetic expression: either a number, or two arithmetic expressions combined by +, x, or ^

#|
Assumptions, based on page 97 descriptions
	- Arithmetic expression means a human-readable infix expression, NOT an executable prefix Scheme expression
	- Book "(quote a)" for the atom "a" is implemented in Scheme as string "a"
	- Book "(quote +)" for the atom "+" (NOT operation "addition") is implemented in Scheme as string "+"
|# 


#|
; tests of data types in various representations

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


(define y '(3 + 2) )
(print-type y) 											; (3 + 2) - List
(print-type (car y))								; 3 - Number
(print-type (car (cdr y))) 					; + - Symbol
(print-type (car (cdr (cdr y)))) 		; 2 - Number
(displayln "")

(define y1 "+")
(print-type y1) 										; + - String
(displayln "")

(define y2 '(+) )
(print-type y2)  										; (+) - List
(print-type (car y2)) 							; + - Symbol
(displayln "")

(define y3 + )
(print-type y3) 										; #<procedure:+> - Procedure
(displayln "")

(define y4 '+ )
(print-type y4)											; + - Symbol
(displayln "")

; this is how to get the data type of operator in the arithmetic expression representation
(define y5 '^ )
(print-type y5)											; ^ - Symbol

(displayln (eq? (car (cdr y)) y4) )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HELPER FUNCTIONS

(define (atom? x)
	(not (list? x)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
; FUNCTION 1
; checks that an aexp contains only those three operators
; input must be in infix notation, and a representation (not list)
(define numbered_1-1?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) '+ ) (and (numbered_1-1? (car aexp)) (numbered_1-1? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) 'x ) (and (numbered_1-1? (car aexp)) (numbered_1-1? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) '^ ) (and (numbered_1-1? (car aexp)) (numbered_1-1? (car (cdr (cdr aexp))))))
		)
	)
)

(define z1 '(3 + (4 x 5)))
; (displayln (numbered_1-1? z1))

(define z2 '(3 + (4 ^ 5)))
; (displayln (numbered_1-1? z2))


;; FUNCTION 2
;; presumes that it's already known that aexp is an arithmetic expression
(define numbered_1-2?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else (and (numbered_1-2? (car aexp)) (numbered_1-2? (car (cdr (cdr aexp))))))
		)
	)
)

(define z3 '(3 + (4 x 5)))
; (displayln (numbered_1-2? z3))

(define z4 '(3 + (4 ^ 5)))
; (displayln (numbered_1-2? z4))


;; FUNCTION 3
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (car (cdr nexp)) '+) (addition (value (car nexp)) (value (car (cdr (cdr nexp))))))
			((eq? (car (cdr nexp)) 'x) (multiply (value (car nexp)) (value (car (cdr (cdr nexp))))))
			(else (expt (value (car nexp)) (value (car (cdr (cdr nexp))))))
		)
	)
)

(define z5 '(1 + (3 ^ 4)) )
(displayln (value z5))

(define z6 '(1 + (3 x 4)) )
(displayln (value z6))


;; FUNCTION 4
; draft of value function that accepts prefix notation
; note: the book specifies this does not do what's intended
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (cdr nexp) '+) (addition (value (cdr nexp)) (value (cdr (cdr nexp)))))
			((eq? (cdr nexp) 'x) (multiply (value (cdr nexp)) (value (cdr (cdr nexp)))))
			(else (expt (value (cdr nexp)) (value (cdr (cdr nexp)))))
		)
	)
)

(define z7 '(1 + (3 x 4)) )
(displayln (value z7)) 					; cdr: contract violation, expected: pair?, given: '()    [expected outcome]


;; FUNCTION 5
; helper function for prefix notation
(define 1st-sub-exp
 	(lambda (aexp)
 		(car (cdr aexp))
 	)
)

(define z1 '(+ 1 3) )
(define z2 '(+ (x 3 6) (^ 3 2)) )

; (displayln (1st-sub-exp z1)) 			; 1
; (displayln (1st-sub-exp z2)) 			; (x 3 6)


;; FUNCTION 7
; helper function for prefix notation
(define operator
	(lambda (aexp)
		(car aexp)
	)
)

; (displayln (operator z1)) 			; +
; (displayln (operator z2)) 			; +


;; FUNCTION 6
; helper function for prefix and infix notation
(define 2nd-sub-exp
	(lambda (aexp)
		(car (cdr (cdr aexp)))
	)
)

; (displayln (2nd-sub-exp z1)) 			; 3 
; (displayln (2nd-sub-exp z2)) 			; (^ 3 2)


;; FUNCTION 8
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (operator nexp) '+) (addition (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
			((eq? (operator nexp) 'x) (multiply (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
			(else (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
		)
	)
)

(printf "~a = ~a\n" z1 (value z1)) 				; (+ 1 3) = 4
(printf "~a = ~a\n" z2 (value z2))				; (+ (x 3 6) (^ 3 2)) = 27


;; FUNCTION 9
;; helper function for infix notation
(define 1st-sub-exp
	(lambda (aexp)
		(car aexp)
	)
)


;; FUNCTION 6
; helper function for prefix and infix notation
(define 2nd-sub-exp
	(lambda (aexp)
		(car (cdr (cdr aexp)))
	)
)


;; FUNCTION 10
(define operator
	(lambda (aexp)
		(car (cdr aexp))
	)
)


;; FUNCTION 8
; now accepts infix notation, since helper functions changed
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (operator nexp) '+) (addition (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
			((eq? (operator nexp) 'x) (multiply (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
			(else (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
		)
	)
)

(define z1 '(3 + (4 x 5)))
(define z2 '(3 + (4 ^ 5)))

(printf "~a = ~a\n" z1 (value z1)) 				; (3 + (4 x 5)) = 23
(printf "~a = ~a\n" z2 (value z2))				; (3 + (4 ^ 5)) = 1027

|#

;; functions 11-15 represent numbers by: () as 0, (()) as 1, (() ()) as two, etc

;; FUNCTION 11
;; tests for zero
(define sero?
	(lambda (n)
		(null? n)
	)
)

(define x0 '() )
(define x1 '(()) )
(define x2 '(() ()) )

; (printf "~a zero? = ~a\n" x0 (sero? x0)) 				; () zero? = #t
; (printf "~a zero? = ~a\n" x1 (sero? x1)) 				; (()) zero? = #f
; (printf "~a zero? = ~a\n" x2 (sero? x2)) 				; (() ()) zero? = #f


;; FUNCTION 12
;; like add1 for this notation
(define edd1
	(lambda (n)
		(cons '() n)
	)
)

; (printf "~a edd1 = ~a\n" x0 (edd1 x0)) 				; () edd1 = (())
; (printf "~a edd1 = ~a\n" x1 (edd1 x1)) 				; (()) edd1 = (() ())
; (printf "~a edd1 = ~a\n" x2 (edd1 x2)) 				; (() ()) edd1 = (() () ())



;; FUNCTION 13
;; like sub1 for this notation
(define zub1
	(lambda (n)
		(cdr n)
	)
)

; (printf "~a zub1 = ~a\n" x0 (zub1 x0)) 				; cdr: contract violation expected: pair? given: '()
; (printf "~a zub1 = ~a\n" x1 (zub1 x1)) 				; (()) zub1 = ()
; (printf "~a zub1 = ~a\n" x2 (zub1 x2)) 				; (() ()) zub1 = (())



;; FUNCTION 14
;; addition for this representation
(define eddition
	(lambda (n m)
		(cond
			((sero? m) n)
			(else (edd1 (eddition n (zub1 m))))
		)
	)
)

(displayln (eddition x1 x2)) 						; (() () ())
(displayln (eddition x1 x0)) 						; (())


(define lat?
	(lambda (l)
		(cond
			((null? l) #t)
			((atom? (car l)) (lat? (cdr l)))
			(else #f)
		)
	)
)

(define y1 '(1 2 3) )
(displayln (lat? y1)) 					; #t

; in the new notation, this would be like (1 2 3)
(define x '( (()) (()()) (()()()) ) )
(displayln (lat? x)) 						; #f, meaning lat? does not recognize the notation

; maybe the point is that functions like lat? make assumptions that numbers are represented by Arabic numerals?