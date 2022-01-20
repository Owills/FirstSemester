;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Part A

;; Examine the following function, and:
;; i.   Write down its signature,
;; ii.  Give it an informative purpose statement, and
;; iii. Give the function and its arguments more informative names.

;; [TODO] Signature
;; detective : Number Number String -> String
;; [TODO] Purpose
;;`Take a persons height and covert to inches. If the person is above 48 inches
;; they are given a message allowing them on a ride.
(define (detective a b c)
  (if (< (+ (* 12 a) b) 48)
      (string-append "Welcome aboard, " c "!")
      "Sorry, you may not board the ride. :("))

;; [TODO] Better names for detective and its arguments
;; 'a' should be 'feet'
;; 'b' should be 'inches'
;; 'c' should be 'name'
;; 'detective' could be 'check-height'

;; Part B

;; Write the signature of the following function:

;; [TODO] Signature
;; mystery : String String Number -> String
(define (mystery x y z)
  (string-append (substring x 0 z)
                 (substring y z)
                 (substring y 0 z)
                 (substring x z)))


;; Part C

;; Describe the values that mystery produces when you apply it to two identical
;; arguments for x and y.

;; [TODO] Prose description as a comment.

;; If arguments x and y are identical the function mystery will return a concatanation of
;; x and y such that the string returned reads the the indentical argument twice in one string,
;; regardless of the value entered for z.


