;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists: a list of strings and a list of
;; numbers. The output should be a list of strings, where each string in the
;; output is the corresponding input string duplicated N times, where N
;; is the number in the corresponding list of numbers. However:
;;
;; 1. If there  are more strings than numbers, assume that the extra strings
;;    should be repeated twice each.
;; 1. If there are more numbers than strings, for each extra number N, 
;;    repeat the the string "Extra!" N times.

;; [TODO] Function design, and you *may* use replicate.

;; repeat-list : [List-of String] [List-of Number] -> [List-of String]
;; produces a list where each string in the given list of strings is repeated N
;; times where N is the corresponding element in the given list of numbers

(check-expect (repeat-list '() '()) '())
(check-expect (repeat-list (list "a" "b") (list 2 3)) (list "aa" "bbb"))
(check-expect (repeat-list (list "a" "b") (list 2 3 4 1)) (list "aa" "bbb"
                                                                "Extra!Extra!Extra!Extra!"
                                                                "Extra!"))
(check-expect (repeat-list (list "a" "b" "c" "d") (list 2 3)) (list "aa" "bbb" "cc" "dd"))

                           
(define (repeat-list los lon)
  (cond
    [(and (empty? los) (empty? lon)) '()]
    [(and (empty? los) (cons? lon))
     (cons (replicate (first lon) "Extra!") (repeat-list los (rest lon)))]
    [(and (cons? los) (empty? lon))
     (cons (replicate 2 (first los)) (repeat-list (rest los) lon))]
    [(and (cons? los) (cons? lon))
     (cons (replicate (first lon) (first los)) (repeat-list (rest los) (rest lon)))]))

