;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

;; Consider the following data definitions (we have deliberately omitted the
;; full data design):

;; A Utility is one of:
;; - "Water"
;; - "Electricity"
;; - "Gas"
;; Interpretation: Represents a kind of utility that a home may receive.

;; A UtilityCompany is one of:
;; - "Eversource"
;; - "National Grid"
;; - "Municipal"
;; Interpretation: Represents a utility company that may provide a utility.

;; Part A

;; What is the most appropriate signature of the following function?

(define (F-4-A x y)
  (cond
    [(and (string=? x "Electricity") (string=? y "National Grid")) #true]
    [(string=? y "Eversource") #true]
    [else #false]))

;; [TODO] Signature

; ;F-4-A : Utility UtilityCompany -> Boolean

;; Part B

;; Write a check-expect where F-4-A produces #false.

(check-expect (F-4-A "Electricity" "Municipal") #false)


