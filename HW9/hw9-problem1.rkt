;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; NOTE #1: You may use list abstractions if you wish. However, they are not
;; mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists of numbers and produces a list of
;; numbers where each number is the average of the two corresponding numbers in 
;; the original lists. You may assume that the two lists have equal length.

;; [TODO]

;; average : [List-of Numbers] [List-of Numbers] -> [List-of Numbers]
;; produces a list where each element of the list is the average of
;; the two corresponding numbers in the original lists

(check-expect (average '() '()) '())
(check-expect (average (list 1 2 3) (list 3 2 1)) (list 2 2 2))

(define (average lox1 lox2)
  (cond
    [(or (empty? lox1) (empty? lox2)) '()]
    [(and (cons? lox1) (cons? lox2))
     (cons (/ (+ (first lox1) (first lox2)) 2) (average (rest lox1) (rest lox2)))]))


