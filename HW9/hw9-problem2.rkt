;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; NOTE #3: You *may not* use the builtin function replicate to solve this
;; problem.

;; Design a function that receives a number N and a list of strings, and
;; produces a list of strings where each output string is the corresponding
;; input string repeated N times.

;; [TODO] Function design *without using replicate*

;; repeat : Nat [List-of String] -> [List-of String]
;; produces a list of strings where each output string is the corresponding
;; input string repeated as many times as inputted.

(check-expect (repeat 5 (list "a" "b")) (list "aaaaa" "bbbbb"))
(check-expect (repeat 5 '()) '())

(define (repeat n los)
  (map (λ (s) (foldr string-append "" (build-list n (λ (x) s)))) los))