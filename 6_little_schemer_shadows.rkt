#lang racket

;;; Chapter 6. Shadows

; arithmetic expression: either a number, or two arithmetic expressions combined by +, x, or ^

;; 1. tests with arithmetic expressions

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

; infix expressions representation: can assign to a variable
; (define x '(3 + 1)) 				; (3 + 1)
; (define x '((3 + 4) * 2) ) 	; ((3 + 4) * 2)
; (displayln x)


; 2. using those expressions in function

; from chatgpt, book does not define this
; define the atom? function
(define (atom? x)
	(not (list? x)))

;; function_2
; simplification of how to determine if an arithmetic expression contains only numbers, +, *, or ^
; presumes that input is an arithmetic expression

; questions
; ? should this return false if there are atoms that are not numbers, or what is it actually checking?
;     - returns false if has a string instead of a number
;     - still returns true if uses a non-operand character
; // does it presume prefix or infix notation, and does it matter?
; 		- seems to check for infix, returns false if given prefix

(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else 
				(and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
		)
	)
)

; prefix expression representation
(define x1 '(* 2 (+ 3 4)) )
(displayln (numbered? x1))  ; #f, prefix invalid

; infix expression representation
(define x2 '((3 + 4) * 2 ) )
(displayln (numbered? x2))  ; #t, infix valid

; infix expression representation with non-number
(define x3 '((3 + 4) * "hi" ) )
(displayln (numbered? x3))  ; #f, string instead of number invalid

; infix expression representation with non-operand
(define x4 '((3 + 4) @ 2 ) )
(displayln (numbered? x4))  ; #t, non-operand character valid


;; function 1
; return to this first implification after figuring out how simplified version runs



#|
;; function 1_1
; debugging version to figure out syntax
; might be this is not working because is infix notation, not prefix as required by racket (eg "(car (cdr aexp))")
; ??? implementation in book seems to leave out the else case to return false, so returned void
; in racket, there is not a built-in operator for exponent like ^, only 'expt', so needs to be called with prefix notation
;   -> omitting from this function for now
; determines whether a representation of arithmetic expression contains only numbers or +
(define numbered_1?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) '(+)  ) (and (numbered_1? (car aexp)) (numbered_1? (car (cdr (cdr aexp))))))
			(else #f)  ; book left this out, so hit void instead
		)
	)
)

(define y 3)
(printf "y: ~a\n" y)
(printf "numbered_1? y: ~a\n\n" (numbered_1? y))  ; #t

(define x '(+ 3 4) )
(printf "x: ~a\n" x)
(printf "numbered_1? x: ~a\n\n" (numbered_1? x))  ; #f

;; function 1_2
; try adjusting to account for prefix notation
; in racket, there is not a built-in operator for exponent like ^, only 'expt', so needs to be called with prefix notation
;   -> omitting from this function for now
; determines whether a representation of arithmetic expression contains only numbers or +
(define numbered_2?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car aexp) '(+)  ) (and (numbered_2? (car aexp)) (numbered_2? (car (cdr aexp)))))  ; if +, check if both car and car of cdr are true
			; ((eq? (car aexp) '(+)  ) (and (numbered_2? (car aexp)) (numbered_2? (car (cdr (cdr aexp))))))  ; if +, check if both car and car of cdr are true
			(else #f)  ; book left this out, so hit void instead
		)
	)
)

(define y 3)
(printf "y: ~a\n" y)
(printf "numbered_2? y: ~a\n\n" (numbered_2? y))  ; #t

(define x '(+ 3 4) )
(printf "x: ~a\n" x)
(printf "numbered_2? x: ~a\n\n" (numbered_2? x))  ; #f

; function_1 has too many irregularities between the book and racket, skipping




(define y 3)
(printf "y: ~a\n" y)
(printf "numbered? y: ~a\n\n" (numbered? y))  ; #t

(define x '(3 + 4) )
(printf "x: ~a\n" x)
(printf "numbered? x: ~a\n\n" (numbered? x))  ; #f

|#
