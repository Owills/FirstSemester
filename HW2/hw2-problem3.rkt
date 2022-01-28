;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; Part A

;; Write a data definition called CompassDirection that represents a single
;; cardinal or inter-cardinal direction that appears on a compass rose:
;;
;; https://en.wikipedia.org/wiki/Cardinal_direction
;;
;; Ensure you follow all steps of the data design recipe.

;; [TODO] Data design recipe
;; A CompassDirection is one of:
;; "North"
;; "Northeast"
;; "East"
;; "Southeast"
;; "South"
;; "Southwest"
;; "West"
;; "Northwest"
;; Interpretation: A cardinal or inter-cardinal direction that appears on a compass rose

(define EX-N "North")
(define EX-NE "Northeast")
(define EX-E "East")
(define EX-SE "Southeast")
(define EX-S "South")
(define EX-SW "Southwest")
(define EX-W "West")
(define EX-NW "Northwest")

(define (compass-template cd)
  (cond
    [(string=? cd EX-N) ...]
    [(string=? cd EX-NE) ...]
    [(string=? cd EX-E) ...]
    [(string=? cd EX-SE) ...]
    [(string=? cd EX-S) ...]
    [(string=? cd EX-SW) ...]
    [(string=? cd EX-W) ...]
    [(string=? cd EX-NW) ...]))
  
;; Part B

;; Write a predicate to determine if a CompassDirection is a cardinal
;; direction (and not an inter-cardinal direction).

;; [TODO] Function design recipe

;; compass-cardinal? : CompassDirection -> Boolean
;; Determines if a CompassDirection is cardinal (true) or an inter-cardinal (false)

;; given EX-N, expect #true
;; given EX-NE, expect #false
;; given EX-SE, expect #false
(define (compass-cardinal? cd)
  (cond
    [(string=? cd EX-N) #true]
    [(string=? cd EX-NE) #false]
    [(string=? cd EX-E) #true]
    [(string=? cd EX-SE) #false]
    [(string=? cd EX-S) #true]
    [(string=? cd EX-SW) #false]
    [(string=? cd EX-W) #true]
    [(string=? cd EX-NW) #false]))

(check-expect (compass-cardinal? EX-N) #true)
(check-expect (compass-cardinal? EX-NE) #false)
(check-expect (compass-cardinal? EX-SE) #false)

;; Part C

;; Write a function that consumes a CompassDirection and produces the opposite
;; direction.

;; compass-opposite-direction : CompassDirection -> CompassDirection
;; returns the opposite CompassDirection
;; given EX-N, expect EX-S
;; given EX-NE, expect EX-SW
;; given EX-SE, expect EX-NW
(define (compass-opposite-direction cd)
  (cond
    [(string=? cd EX-N) EX-S]
    [(string=? cd EX-NE) EX-SW]
    [(string=? cd EX-E) EX-W]
    [(string=? cd EX-SE) EX-NW]
    [(string=? cd EX-S) EX-N]
    [(string=? cd EX-SW) EX-NE]
    [(string=? cd EX-W) EX-E]
    [(string=? cd EX-NW) EX-SE]))

(check-expect (compass-opposite-direction EX-N) EX-S)
(check-expect (compass-opposite-direction EX-NE) EX-SW)
(check-expect (compass-opposite-direction EX-SE) EX-NW)



