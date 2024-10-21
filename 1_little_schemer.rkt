#lang racket

; Chapter 1. Toys

#|
(define my-list '(1 "two" 3.0 #t))

(if (list? my-list)
  (displayln "is a list")
  (displayln "not a list")
)

;(for ([item my-list])
;  (displayln item))


(for ([item my-list])
  (display item) ; this avoids type check to concat with whitespace
  (display ": ")
  (cond
    [(number? item) (displayln "number")]
    [(string? item) (displayln "string")]
    [(boolean? item) (displayln "boolean")]
    [else (displayln "Unknown type")]
  )
)

(define empty-list '())
(if (list? empty-list)
  (displayln "empty list is a list")
  (displayln "empty list is not a list")
)

(define my-list-1 '((("how") "are") (("you") ("doing so")) "far"))
(for ([item my-list-1])
  (displayln item))
(displayln "")

;(displayln (car my-list-1))
;(displayln (cdr my-list-1))

(define item-1 (car my-list-1))
(displayln item-1)

; conditional with ternary operator
(for ([item item-1])
  (cond
    [(list? item) (displayln "is a list") (displayln "not a list")]
  )
)

; same conditional with if
(for ([item item-1])
  (if (list? item) (displayln "still is a list") (displayln "still not a list"))
)

(define my-list-2 '(("a" "b" "c") "x" "y" "z"))
(define car-2 (car my-list-2))
(displayln car-2)
(define cdr-2 (cdr my-list-2))
(displayln cdr-2)
displayln cdr-2

; surprise about printing: displayln not in parens is not an error,
; but outputs way list was defined, rather than pretty print
; eg (displayln cdr-2)
;     -->  (x y z)
; eg displayln cdr-2
;      -->  #<procedure:displayln>
;           '("x" "y" "z")

(define a "peanut")
(define l '("butter" "and" "jelly"))
(define a-l (cons a l))
(displayln a-l)

(define s '("banana" "and"))
(define l '("peanut" "butter" "and" "jelly"))
(define s-l (cons s l))
(displayln s-l)  ;  ((banana and) peanut butter and jelly)

|#

(define e '())
(displayln (null? e))  ; #t

(define a "peanut")
(displayln (null? a))  ; #f

(define p "apple")
(define l '("apple"))
(displayln (eq? p l))
(displayln (eq? (car l) p)) ; #t