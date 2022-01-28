;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; Consider the following data definitions and interpretations.

(define-struct sneaker [is-running])
;; A ShoeKind is one of:
;; - (make-sneaker Boolean)
;; - "dress"
;; - "boots"
;; - "other"
;; Interpretation: Represents some shoe varieties. In (make-sneaker r),
;; r is #true when the sneaker is a running shoe. The "other"
;; variety is used for the many other kinds of shoes that aren't listed.

(define N-1 (make-sneaker #true))
(define N-2 (make-sneaker #false))
(define N-3 "dress")
(define N-4 "boots")
(define N-5 "other")

(define-struct shoe [brand laced kind])
;; A Shoe is a (make-shoe String Boolean ShoeKind).
;; Interpretation: A (make-shoe b l k) represents a shoe made by b,
;; of kind k, which has laces when l is #true, and is a slip-on shoe
;; when l is #false.

;; Part A

;; Define five distinct examples of Shoe data. Make sure that your
;; examples are representative, and are distinct enough to cover all the
;; interesting differences in this data.

;; [TODO] Five examples

(define S-1 (make-shoe "Nike" #true N-1))
(define S-2 (make-shoe "Adidas" #false N-2))
(define S-3 (make-shoe "Nike" #true N-3))
(define S-4 (make-shoe "Adidas" #true N-4))
(define S-5 (make-shoe "Puma" #false N-5))



;; Part B

;; Write templates for all data definitions above.

;; [TODO] Templates

;; sneak-temp: ShoeKind -> ???
(define (sneak-temp n)
  (cond
    [(sneaker? n) ...]
    [(string=? "boots" n) ...]
    [(string=? "dress" n) ...]
    [(string=? "other" n) ...]))

;; shoe-temp : Shoe -> ???
(define (shoe-temp s)
  (... (shoe-brand s) ...
       (shoe-laced s) ...
       (sneak-temp (shoe-kind s)) ...))

;; Part C

;; Consider the following function definitions:

;; foo: ShoeKind -> String
(define (foo u)
  (cond
    [(sneaker? u)
     (if (sneaker-is-running u)
         "running shoes"
         "sneakers")]
    [(string=? u "dress") "dress shoes"]
    [(string=? u "boots") "boots"]
    [(string=? u "other") ""]))                           

;; bar: Shoe -> String
(define (bar t)
  (string-append (foo (shoe-kind t)) " by " (shoe-brand t)))

;; Write the signatures for the functions foo and bar.

;; Hint: The names "foo", "bar", "u", and "t" are not helpful. It may help you
;; to come up with better names as well. However, this is completely optional.
;; If you do so, feel free to change the names in the code above.

;; [TODO] Better signatures for foo and bar


