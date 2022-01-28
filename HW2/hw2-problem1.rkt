;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Design a function that consumes two strings and produces a single string
;; that concatenates them, with a space between them. However, in this result,
;; the first string should be the *longer* of the two. Ensure you follow
;; all steps of the design recipe, and include three distinct check-expects.

;; [TODO]
;; combine-longer-then-shorter-string ; string string -> string
;; Takes two strings, concatonating them with the longer string first,
;; then " ", followed by the shorter string
;; given "Oliver" "Hello", expect "Oliver Hello"
;; given "Oliver" "Hello Hello", expect "Hello Hello Oliver"
;; given "space" "check-expect", "check-expect space"
(define (combine-longer-then-shorter-string str1 str2)
  (if (>= (string-length str1) (string-length str2))
      (string-append str1 " " str2)
      (string-append str2 " " str1)))

(check-expect (combine-longer-then-shorter-string "Oliver" "Hello") "Oliver Hello")
(check-expect (combine-longer-then-shorter-string "Oliver" "Hello Hello") "Hello Hello Oliver")
(check-expect (combine-longer-then-shorter-string "space" "check-expect") "check-expect space")