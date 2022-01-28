;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; NOTE: In this problem (and all subsequent problems), for every function
;; design, you must write (1) *at least* two tests, and (2) ensure that there
;; are no "halloween colors". To address the latter, you may have to write
;; additional tests.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the three functions below (we have deliberately omitted tests):

;; hello-everyone: [List-of String] -> [List-of String]
;; Greets everyone in the list.
(define (hello-everyone los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append "Hello, " (first los) "!")
                       (hello-everyone (rest los)))]))

(check-expect (hello-everyone '()) '())
(check-expect (hello-everyone (list "Jeff" "Barry")) (list "Hello, Jeff!" "Hello, Barry!"))

;; words-until-period: [List-of String] -> [List-of String]
;; Produces the words in the list up to the first period.
(define (words-until-period los)
  (up-to str-helper los))

(check-expect (words-until-period '()) '())
(check-expect (words-until-period (list ".")) '())
(check-expect (words-until-period (list "Hello" "My" "Name" "Is" "." "Hi"))
              (list "Hello" "My" "Name" "Is"))


;; starting-positive-numbers : [List-of Number] -> [List-of Number]
;; Produces the prefix of positive numbers in the list.
(define (starting-positive-numbers lon)
  (up-to pos-helper lon))

(check-expect (starting-positive-numbers '()) '())
(check-expect (starting-positive-numbers (list 10 20 -1 10)) (list 10 20))
(check-expect (starting-positive-numbers (list 40 1 32)) (list 40 1 32))


;; Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

;; [TODO] Design a list abstraction.

;; up-to : (X) [X -> Boolean] [List-of X] -> [List-of X]
;; Produces elements in a list up to when a given element or type of element is presented.
(define (up-to the-func alist)
  (cond
    [(empty? alist) '()]
    [(cons? alist) 
     (if (the-func (first alist))
         '()
         (cons (first alist) (up-to the-func (rest alist))))]))

(check-expect (up-to pos-helper '()) '())
(check-expect (up-to pos-helper (list 2 4 5 -6 -32)) (list 2 4 5))
(check-expect (up-to str-helper (list "Hi" "." "Bye" ".")) (list "Hi"))

;; pos-helper : Number -> Boolean
;; Checks if value is less than or equal to 0
(define (pos-helper s)
  (<= s 0))

(check-expect (pos-helper 5) #false)
(check-expect (pos-helper -2) #true)

;; str-helper : String -> Boolean
;; Checks if value is equal to "."
(define (str-helper s)
  (string=? s "."))

(check-expect (str-helper "hello") #false)
(check-expect (str-helper ".") #true)



;; Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can.


