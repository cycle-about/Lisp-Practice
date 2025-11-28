#lang racket

;;; Chapter 9. ...and Again, and Again, and Again,...
;;; caution: this chapter addresses the halting problem and non-terminating functions
;;; consider that when implemeting functions or not 

;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;

; return the item at index n, from chapter 4
; scheme is 1-indexed, index 0 is an error
(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(else (pick (sub1 n) (cdr lat)))
		)
	)
)


;;;;;;;;;;;; END HELPER FUNCTIONS ;;;;;;;;;;;;

;; FUNCTION 1
;; page 150 but used by function 2
;; sorn stands for symbol or number
;; unusual keep-looking does not recur on part of lat, instead on whole of lat, so unnatural recursion
;; 
(define keep-looking
	(lambda (a sorn lat)
		(cond
			((number? sorn)
				(keep-looking a (pick sorn lat) lat)
			)
			(else
				(eq? sorn a)
			)
		)
	)
)

(define a1 'caviar)
(define lat1 '(6 2 4 caviar 5 7 3) ) ; don't use quote char on string when in a list

; (displayln (keep-looking a1 3 lat1 )) ; #t
; (displayln (keep-looking a1 4 lat1 )) ; #t
; my additional tests
; (displayln (keep-looking a1 1 lat1 )) ; #t
; (displayln (keep-looking 'grits 1 lat1 )) 	; #f
; (displayln (keep-looking a1 6 lat1 )) ; #t


;; FUNCTION 2
;; actually on p 149, but uses function 1
(define looking
	(lambda (a lat)
		(keep-looking a (pick 1 lat) lat)
	)
)

(define lat2 '(6 2 grits caviar 5 7 3) )

(displayln "Function 2")
(displayln (looking a1 lat1)) ; #t
(displayln (looking a1 lat2)) ; #f