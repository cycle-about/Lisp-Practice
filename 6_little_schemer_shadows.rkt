#lang racket

;;; Chapter 6. Shadows

#|
; arithmetic expression: either a number, or two arithmetic expressions combined by +, x, or ^
; note: racket does NOT include an operator for exponent (eg ^), only function 'expt'
;     so not sure if the functions 'numbered_1?' will work as intended

;; Tests with arithmetic expressions in racket

;; prefix expression value: can assign to a variable
; (define x 3) 								; 3
; (define x (+ 3 1)) 					; 4
; (define x (* 2 (+ 3 4)) ) 	; 14
; (displayln x)

;; prefix expression representation: can assign to a variable
; (define x '(3) ) 						; (3)
; (define x '3 ) 							; 3
; (define x '(+ 3 1)) 				; (+ 3 1)
; (define x '(* 2 (+ 3 4)) ) 	; (* 2 (+ 3 4))
; (displayln x)

; infix expression value: CANNOT assign to a variable
; (define x (3 + 1)) 					; application: not a procedure; expected a procedure that can be applied to arguments
; (define x ((3 + 4) * 2) ) 	; application: not a procedure; expected a procedure that can be applied to arguments
; (displayln x)

; infix expression value without parens: bad syntax
; (define x 3 + 1) 							; bad syntax (multiple expressions after identifier)

; infix expressions representation: can assign to a variable
; (define x '(3 + 1)) 				; (3 + 1)
; (define x '((3 + 4) * 2) ) 	; ((3 + 4) * 2)
; (displayln x)


;; function_2
;; doing before function_1 since it's the simplified version

; define the atom? function
(define (atom? x)
	(not (list? x)))

; simplification of how to determine if an arithmetic expression contains only numbers, +, *, or ^
; presumes that input is an arithmetic expression
; note: Racket does not have operator for exponent, so this might not do what's intended

#|
(define numbered_1?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else 
				(and (numbered_1? (car aexp)) (numbered_1? (car (cdr (cdr aexp))))))
		)
	)
)

;; Question 1. What does function_2 presume for prefix/infix and value/representation
;    -> infix representation only

; prefix value: NOT valid input to numbered_1?
(define x1 (+ 3 1))
(printf "prefix value: ~a\n" x1) 							; 4
(printf "numbered_1?: ~a\n\n" (numbered_1? x1)) 	; #t  -> this runs the function on 4, so does NOT check what's intended

; prefix representation: NOT valid input to numbered_1?
(define x2 '(+ 3 1))
(printf "prefix representation: ~a\n" x2) 		; (+ 3 1)
(printf "numbered_1?: ~a\n\n" (numbered_1? x2))  	; #f -> function does not see this as valid

; infix value: cannot be evaluated by Racket, so not valid for numbered_1?

; infix representation: valid input
(define x4 '(3 + 4) )
(printf "infix representation: ~a\n" x4) 			; (3 + 4)
(printf "numbered_1?: ~a\n\n" (numbered_1? x4)) 	; #t

(define x5 '((3 + 4) * 2 ) )
(printf "infix representation: ~a\n" x5) 			; ((3 + 4) * 2)
(printf "numbered_1?: ~a\n\n" (numbered_1? x5)) 	; #t


;; Question 2. What operators and operands does function_2 check for?
; 	-> Seems to allow anything, string, empty list, another number in between numerics
;     but requires three or more atoms, with two or more numbers, and position of number matters
;   -> This function does NOT evaluate whether something is actually a 'legal' arithmetic expression
;     in person-facing infix notation

; (define x6 '(3 $ 2 ))  		; #t
; (define x6 '(3 @ 2 ))  		; #t
; (define x6 '(3 b 2 )) 		; #t
; (define x6 '(3 "b" 2)) 		; #t
; (define x6 '(3 '() 2)) 		; #t
; (define x6 '(3 1 2)) 			; #t
; (define x6 '("hi")) 			; #false
; (define x6 '(3 "hi")) 		; error contract violation expected pair? given '()
; (define x6 '(3 2))				; error contract violation expected pair? given '()
; (define x6 '(3))					; error contract violation expected pair? given '()
; (define x6 '(3 2 1 4))		; #t
; (define x6 '("hi" "bye" "hello"))				; #f
; (define x6 '(3 "bye" "hello"))					; #f
(define x6 '(3 2 "hello"))							; #f
(displayln x6)
(printf "numbered_1?: ~a\n\n" (numbered_1? x6)) 	; 
|#


;; function_1

; pre-req for function: equality test on operators

#|
;; initial tests of equals
; (define x2 +)							; x1=+  x2=#<procedure:+>  #f
(define x2 "+") 						; x1=+  x2=+  #f

(printf "x1 = ~a  " x1)

(printf "\nx2=~a  " x2)
(displayln (eq? x1 x2))
|#


;; explicitly get data types
(define (print-type obj)
  (cond
    [(number? obj) (displayln "Number")]
    [(string? obj) (displayln "String")]
    [(boolean? obj) (displayln "Boolean")]
    [(symbol? obj) (displayln "Symbol")]
    [(list? obj) (displayln "List")]
    [(procedure? obj) (displayln "Procedure")]
    [else (displayln "Unknown type")]))

; note: assigning to variable does not affect the type

(define x1 '(3 + 2))
(printf "~a  " x1)
(print-type x1) 					; (3 + 2)  List

(define x2 (car (cdr x1)))
(printf "~a  " x2)
(print-type x2) 					; +  Symbol

(define x3 +)
(printf "~a  " x3)
(print-type x3) 					; #<procedure:+>  Procedure

(define x4 '(+) )
(printf "~a  " x4)
(print-type x4) 		 			; (+)  List

; this one is surprising
(define x5 (+) )
(printf "~a  " x5)
(print-type x5) 		 			; 0  Number


(define numbered_1?
	(lambda aexp
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) (+)))
		)
	)
)

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;; RESTART 


#|
Assumptions, based on page 97 descriptions
	- Arithmetic expression means a human-readable infix expression, NOT an executable prefix Scheme expression
	- Book "(quote a)" for the atom "a" is implemented in Scheme as string "a"
	- Book "(quote +)" for the atom "+" (NOT operation "addition") is implemented in Scheme as string "+"
|#

(define (atom? x)
	(not (list? x)))


;; function 1 version 1
(define numbered_1-1?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) ("+")) (and (numbered_1-1? (car aexp)) (numbered_1-1? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) ("x")) (and (numbered_1-1? (car aexp)) (numbered_1-1? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) ("^")) (and (numbered_1-1? (car aexp)) (numbered_1-1? (car (cdr (cdr aexp))))))
		)
	)
)
 	
; (define y (3 + (4 x 5))) 			; x: unbound identifier
(define y1 '(3 + (4 x 5)))
(define y2 "(3 + (4 x 5))")
(define y3 "3 + 4 x 5")

(displayln "results for numbered_1-1")
(displayln (numbered_1-1? y1))		; application: not a procedure; expected a procedure that can be applied to arguments given: "+"
(displayln (numbered_1-1? y2)) 		; #f, expect true though
(displayln (numbered_1-1? y3)) 		; #f, expect true though


;; function 1 version 2
(define numbered_1-2?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) "+") (and (numbered_1-2? (car aexp)) (numbered_1-2? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) "x") (and (numbered_1-2? (car aexp)) (numbered_1-2? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) "^") (and (numbered_1-2? (car aexp)) (numbered_1-2? (car (cdr (cdr aexp))))))
		)
	)
)

(displayln "results for numbered_1-2")
; (displayln (numbered_1-2? y1))			; #<void>
; (displayln (numbered_1-2? y2))			; x: unbound identifier
; (displayln (numbered_1-2? y3)) 			; #f, expect true though


;; function 1_v2
(define numbered_1-3?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) '("+")) (and (numbered_1-3? (car aexp)) (numbered_1-3? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) '("x")) (and (numbered_1-3? (car aexp)) (numbered_1-3? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) '("^")) (and (numbered_1-3? (car aexp)) (numbered_1-3? (car (cdr (cdr aexp))))))
		)
	)
)
				
; (displayln (numbered_1-2? y1))			; #<void>
; (displayln (numbered_1-2? y2))			; x: unbound identifier
; (displayln (numbered_1-2? y3)) 			; #f, expect true though


;; moving on from function 1, unclear how to implement


;; function 2
(define numbered_3? 
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else (and (numbered_3? (car aexp)) (numbered_3? (car (cdr (cdr aexp))))))
		)
	)
)

; (displayln (numbered_3? y1))			; #t
; (displayln (numbered_3? y2))			; #f
; (displayln (numbered_3? y3))			; #f

;; additional tries with function_2 and syntax of variable definition that worked

(define y4 '(2 x sausage))
; (displayln (numbered_3? y4))			; #f, expected answer

(define y5 '(3 + (4 ^ 5)))
; (displayln (numbered_3? y5))			; #t, expected answer
; (displayln (numbered_2? y5))			; #<void>
; (displayln (numbered_1? y5))			; expected a procedure that can be applied to arguments given: "+"

;; conclusion: chapter's function 2 might be implemented correctly, but function 1 does not seem to evaluate the same way

;; function 3
(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (car (cdr nexp)) "+") (addition (value (car nexp)) (value (car (cdr (cdr nexp))))))
			((eq? (car (cdr nexp)) "x") (multiply (value (car nexp)) (value (car (cdr (cdr nexp))))))
			(else (expt (value (car nexp)) (value (car (cdr (cdr nexp))))))s
		)
	)
)