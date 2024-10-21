#lang racket

#|
(displayln "3. Basic Arithmetic Operations")
(displayln (+ 3 2)) ; this is a comment
(displayln (- 5 3))
; 'number->string' is the function name, arrow is convention to indicate transformation
(displayln (string-append "- 5 3 = " (number->string (- 5 3))))
(displayln (string-append "* 2 4 = " (number->string (* 2 4))))

(displayln "\n4. Working with Variables")
(define x 10)
(define y 20)
(displayln (string-append "+ x y = " (number->string (+ x y))))

(displayln "\n5. Defining and Calling Functions")
(define (add-two-numbers a b)
  (+ a b))
(displayln (add-two-numbers 5 7))

(displayln "\n6. Using conditionals")
(define age 10)
(if (>= age 18)
  (displayln "You are an adult") ; executes if true
  (displayln "You are a minor")) ; executes if false

(displayln "\n7. Calculating Factorials with Recursion")
; no explicit return statements
(define (factorial n)
  (if (<= n 1)
    1				; base case, when n equal or under 1
    (* n (factorial (- n 1)))))	; recursive case
(displayln (factorial 5))

(displayln "\n8. List Operations")
; single quote before paren prevents it from being evaluated as function/expression, marking it as a literal list
; without the quote, would mean calling function "1" with args "2 3 4 5"
(define my-list '(1 2 3 4 5))
(displayln my-list)
(displayln (car my-list))
(displayln (cdr my-list))
(define new-list-1 (cons 0 my-list))
(displayln new-list-1)
(define new-list-2 (append my-list '(6)))
(displayln new-list-2)

(define str-list '("one" "two" "three" "four"))
(displayln str-list)
(define str-list-1 (cons 0 str-list))
(displayln str-list-1)
(define str-list-2 (append str-list-1 '("five")))
(displayln str-list-2)

; String: enclosed in double quote, represent textual data
; Symbol: used without quotes or preceded with single quote, represent unique identifiers
|#

(displayln "\n9. Using loops with 'for'")
;(for ([item '(10 20 30 40)])
;  (displayln item))
(define l '(10 20 30 40))
(for ([item l])
      (displayln item))
