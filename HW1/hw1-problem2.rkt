;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; You and your friend have been arguing about how best to invest some money.
;; You think you've picked some stocks that give decent gains consistently, but
;; your friend really wants to invest in cryptocurrencies, which she believes
;; have substantially larger gains some good days, but also suffer some losses
;; on some bad days. To settle the debate of which to invest in, you offer to 
;; program a simulation of the two choices.

;; Part A

;; Define a function stock-day which simulates a single day of gain from the
;; stocks you have in mind. Specifically, stock-day should receive as its
;; argument the amount of money you have, and should produce the amount you
;; will have after a 4% gain. For example, (stock-day 1000) should produce 1040.
;; In addition, write three examples. Here is one to get you started:
;;
;; (stock-day 1000) ; produces 1040

;; [TODO] Function definition
;; stock-day : Number -> Number
;; Multiplies input by 1.04
(define (stock-day starting-money)
  (* starting-money 1.04))

;; [TODO] Examples
;; (stock-day 4764) ; produces 4954.56
;; (stock-day 20) ; produces  20.8
;; (stock-day 715) ; produces 743.6

;; Part B

;; Define a function crypto-good-day which simulates a single day of gain from
;; cryptocurrencies, assuming it was a good day. Specifically, crypto-good-day
;; should calculate the amount of money you will have after a 10% gain.
;; You must also write three examples of this function.

;; [TODO] Function definition
;; crypto-good-day : Number -> Number
;; Multiplies input by 1.1
(define (crypto-good-day starting-money)
  (* starting-money 1.1))

;; [TODO] Three examples
;; (crypto-good-day 1000) ; produces 1100
;; (crypto-good-day 4764) ; produces 5240.4
;; (crypto-good-day 20) ; produces 22

;; Part C

;; Define a function crypto-bad-day which simulates a single day of loss from
;; cryptocurrencies, assuming it was a bad day. Specifically, crypto-bad-day
;; should compute the total amount of money you will have after a –2% loss.
;; For example, if you start with $100 in crypto, after a bad day, you will
;; have $98 left. You must also write three examples for this function.

;; [TODO] Function definition
;; crypto-bad-day : Number -> Number
;; Multiplies input by 0.98
(define (crypto-bad-day starting-money)
  (* starting-money 0.98))

;; [TODO] Three examples
;; (crypto-bad-day 1000) ; produces 980
;; (crypto-bad-day 4764) ; produces 4668.72
;; (crypto-bad-day 20) ; produces 19.6


;; Part D
;;
;; Define a constant STOCK-6-DAYS which is the total amount of money you would
;; have after starting with $1000 and repeatedly investing all of it in stocks
;; six days. You must use the stock-day function.
;;
;; Hint: You can use the value produced by stock-day on the first day as the
;; argument for stock-day on the second day, and so on.

;; [TODO] Define the constant
(define STOCK-6-DAYS (stock-day (stock-day (stock-day (stock-day (stock-day (stock-day 1000)))))))


;; Define a constant CRYPTO-6-DAYS which  simulates cryptocurrency trading for
;; 6 days starting with $1,000, alternating crypto-good-day and crypto-bad-day,
;; **starting with crypto-good-day**.

;; [TODO] Define the constant
(define CRYPTO-6-DAYS (crypto-bad-day
                       (crypto-good-day
                        (crypto-bad-day
                         (crypto-good-day
                          (crypto-bad-day
                           (crypto-good-day 1000)))))))

;; Part E
;;
;; Now compare the results! Which one seems to have done better?


;; [TODO] Write which one seems better? Write it as a comment.

;; CRYPTO-6-DAYS equals 1252.726552
;; STOCK-6-DAYS equals 1265.319018496
;; since STOCK-6-DAYS > CRYPTO-6-DAYS investing 1000 dollars in the stock market seems
;; to have done better over 6 days


