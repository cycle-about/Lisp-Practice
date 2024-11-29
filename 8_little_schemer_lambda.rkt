#lang racket

;;; Chapter 8. Lambda the Ultimate

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

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

;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; FUNCTION 1
;; version of rember that takes the equality tets as an arg (eq?, equal?, or =)
(define rember-f1
	(lambda (test? a l)
		(cond
			((null? l) '() )
			((test? (car l) a) (cdr l))
			(else (cons (car l) (rember-f1 test? a (cdr l))))
		)
	)
)

(define t1 =)
(define a1 5)
(define l1 '(6 2 5 3) )

; (displayln (rember-f1 t1 a1 l1))  ; (6 2 3)


;; Curry-ing: a function that when passed an argument a, returns another function with a as its arg

;; FUNCTION 2
(define eq?-c
	(lambda (a)
		(lambda (x)
			(eq? x a)
		)
	)
)

; (print-type (eq?-c 3))   ; #<procedure:..._schemer_lambda.rkt:43:16> - Procedure
; [printout does not include name of function, only line numbers and file name]


;; FUNCTION 3
(define rember-f2
	(lambda (test?)
		(lambda (a l)
			(cond
				((null? l) '() )
				((test? (car l) a) (cdr l))
				(else (cons (car l) ((rember-f2 test?) a (cdr l))))
			)
		)
	)
)

(define a2 "tuna")
(define l2 '("shrimp" "salad" "and" "tuna" "salad") )
; (displayln ((rember-f2 eq?) a2 l2))     ; (shrimp salad and salad)


;; FUNCTION 4
;; make a version of insertL that also takes the equality function as an arg
; original insertL in chapter 3
(define insertL-f
	(lambda (test?)
		(lambda (new old l)
			(cond
				((null? l) '() )
				((test? (car l) old) (cons new (cons old (cdr l))))
				(else (cons (car l) ((insertL-f test?) new old (cdr l))))
			)
		)
	)
)

(define l3 '("chips" "and" "fish" "or" "fish" "and" "fried"))
(define n1 "fried")
(define o1 "fish")
; (displayln ((insertL-f eq?) n1 o1 l3))  ; (chips and fried fish or fish and fried)


;; FUNCTION 5
(define insertR-f
	(lambda (test?)
		(lambda (new old l)
			(cond
				((null? l) '() )
				((test? (car l) old) (cons old (cons new (cdr l))))
				(else (cons (car l) ((insertR-f test?) new old (cdr l))))
			)
		)
	)
)

; (displayln ((insertR-f eq?) n1 o1 l3))  ; (chips and fish fried or fish and fried)

;; FUNCTION 6
;; helper function that defines where the cons is different in insertL from insertR
(define seqL
	(lambda (new old l)
		(cons new (cons old l))
	)
)

;; FUNCTION 7
;; helper function for the cons in insertR
(define seqR
	(lambda (new old l)
		(cons old (cons new l))
	)
)

;; FUNCTION 8
(define insert-g
	(lambda (seq)
		(lambda (new old l)
			(cond
				((null? l) '() )
				((eq? (car l) old) (seq new old (cdr l)))
				(else (cons (car l) ((insert-g seq) new old (cdr l))))
			)
		)
	)
)

; (displayln ((insert-g seqL) n1 o1 l3))  ; (chips and fried fish or fish and fried)


;; FUNCTION 9
(define insertL-1 
	(insert-g 
		seqL 					; this is a named function, in place of a lambda
	)
)

; (displayln (insertL-1 n1 o1 l3))  ; (chips and fried fish or fish and fried)


;; it is not necessary to define functions to pass them as args, can pass the definitions themselves (page 132)
;; eg the arg seqL versus seqR is the only difference between revised insertL and insertR, so not useful as standalone functions


;; FUNCTION 10
;; insertL with an anonymous function, instead of seqL
(define insertL-2
	(insert-g
		(lambda (new old l)				; these three lines are a lambda (anonymous function), instead of named function 'seqL' as in insertL-1
			(cons new (cons old l))
		)
	)
)

(displayln (insertL-2 n1 o1 l3))  ; (chips and fried fish or fish and fried)

;; FUNCTION 11
;; insertR with anonymous function
(define insertR-2
	(insert-g
		(lambda (new old l)				; these three lines are a lambda (anonymous function), instead of named function
			(cons old (cons new l))
		)
	)
)

(displayln (insertR-2 n1 o1 l3))  ; (chips and fish fried or fish and fried)