#lang racket

;;; Chapter 6. Shadows

; arithmetic expression: either a number, or two arithmetic expressions combined by +, x, or ^
; note: racket does NOT include an operator for exponent (eg ^), only function 'expt'
;     so not sure if the functions 'numbered?' will work as intended

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
(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else 
				(and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
		)
	)
)

;; Question 1. What does function_2 presume for prefix/infix and value/representation
;    -> infix representation only

; prefix value: NOT valid input to numbered?
(define x1 (+ 3 1))
(printf "prefix value: ~a\n" x1) 							; 4
(printf "numbered?: ~a\n\n" (numbered? x1)) 	; #t  -> this runs the function on 4, so does NOT check what's intended

; prefix representation: NOT valid input to numbered?
(define x2 '(+ 3 1))
(printf "prefix representation: ~a\n" x2) 		; (+ 3 1)
(printf "numbered?: ~a\n\n" (numbered? x2))  	; #f -> function does not see this as valid

; infix value: cannot be evaluated by Racket, so not valid for numbered?

; infix representation: valid input
(define x4 '(3 + 4) )
(printf "infix representation: ~a\n" x4) 			; (3 + 4)
(printf "numbered?: ~a\n\n" (numbered? x4)) 	; #t

(define x5 '((3 + 4) * 2 ) )
(printf "infix representation: ~a\n" x5) 			; ((3 + 4) * 2)
(printf "numbered?: ~a\n\n" (numbered? x5)) 	; #t


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
(printf "numbered?: ~a\n\n" (numbered? x6)) 	; 
|#

