;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; NOTE: For all function designs, you must have 2+ tests and no halloween
;; colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; The final objective in this problem is to design these three functions:
;;
;; 1. hello-goodbye: given a list of names, produces two strings for each
;;    string NAME: "Hello NAME!" and "Goodbye NAME!".
;;
;; 2. double-double: given a list of numbers, produces a list with two numbers
;;    for each number x: (* 2 x) and (* 4 x).
;;
;; 3. string-length-length : given a list of strings, produces a list with two 
;;    numbers for each string: the length, followed by half the length.
;;
;; However, you must not use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.
;;
;; To help you get started, here are the tests for the three functions.
;; However, you should also test your abstraction independently.

(check-expect (hello-goodbye '()) '())
(check-expect
 (hello-goodbye (cons "Alice" (cons "Bob" '())))
 (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))

(check-expect (double-quad '()) '())
(check-expect (double-quad (cons 10 (cons 20 '())))
              (cons 20 (cons 40 (cons 40 (cons 80 '())))))

(check-expect (normal-half-length '()) '())
(check-expect (normal-half-length (cons "Hello" '()))
              (cons 5 (cons 2.5 '())))


;; hello-goodbye : [List-of String] -> [List-of String]
;; produces two strings for each string NAME: "Hello NAME!" and "Goodbye NAME!".
(define (hello-goodbye los)
  (two-values los add-hello add-goodbye))

;; double-quad : [List-of Number] -> [List-of Number]
;; produces a list with two numbers for each number x: (* 2 x) and (* 4 x).
(define (double-quad lon)
  (two-values lon double quad))

;; normal-half-length : [List-of String] -> [List-of Number]
;; produces a list with two numbers for each string: the length, followed by half the length.
(define (normal-half-length los)
  (two-values los string-length string-length-half))


;; two-values : (X Y) [List-of X] [List-of X -> List-of Y] [List-of X -> List-of Y] -> [List-of Y]
;; given a list creates a new lest with two element for each element in the given list
;; based on the parameters of the helper functions
(define (two-values lox f1 f2)
  (cond
    [(empty? lox) '()]
    [(cons? lox) 
     (cons (f1 (first lox))
           (cons (f2 (first lox))
                 (two-values (rest lox) f1 f2)))]))



(check-expect (two-values (cons 1 (cons 2 '())) double quad)
              (cons 2 (cons 4 (cons 4 (cons 8 '())))))

(check-expect (two-values (cons "Ved" (cons "Oliver" '())) add-hello add-goodbye)
              (cons "Hello Ved!"
                    (cons "Goodbye Ved!"
                          (cons "Hello Oliver!" (cons "Goodbye Oliver!" '())))))

(check-expect (two-values (cons "Bye" '()) string-length string-length-half)
              (cons 3 (cons 1.5 '())))


;; double : Number -> Number
;; multiplies the given number by 2
(define (double number)
  (*  number 2))

(check-expect (double 0) 0)
(check-expect (double 69) 138)

;; quad : Number -> Number
;; multiplies the given number by 4
(define (quad number)
  (*  number 4))

(check-expect (quad 0) 0)
(check-expect (quad 69) 276)

;; string-length-half : String -> Number
;; get the length of the string multiplies it by 0.5
(define (string-length-half s)
  (*  (string-length s) 0.5))

(check-expect (string-length-half "number") 3)
(check-expect (string-length-half "69") 1)

;; add-hello : String -> String
;; given a string NAME produces: "Hello NAME!"
(define (add-hello s)
  (string-append "Hello " s "!"))

(check-expect (add-hello "number") "Hello number!")
(check-expect (add-hello "69") "Hello 69!")

;; add-goodbye : String -> String
;; given a string NAME produces: "Goodbye NAME!"
(define (add-goodbye s)
  (string-append "Goodbye " s "!"))

(check-expect (add-goodbye "number") "Goodbye number!")
(check-expect (add-goodbye "69") "Goodbye 69!")


  



