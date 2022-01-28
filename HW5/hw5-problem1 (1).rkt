;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw5-problem1 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the following data definition:

;; A ListofString (LoS) is one of:
;; - '()
;; - (cons String LoS)
;; Interpretation: A list of strings.

(define EX-LOS-1 '())
(define EX-LOS-2 (cons "susie"
                       (cons "robert baratheon"
                             (cons "alice" EX-LOS-1))))
(define EX-LOS-3 (cons "bob" EX-LOS-2))

;; los-template: LoS -> ?
(define (los-template los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) (... (first los) ...
                       (los-template (rest los)) ...)])))

;; Part A

;; Design a predicate called contains-three-letter-string? that consumes an LoS 
;; and determines if it contains a string that is exactly three letters long.

;; [TODO] Function design

;; contains-three-letter-string? : LoS --> Boolean
;; determines if an LoS contains a string that is exactly three letters long.
(define (contains-three-letter-string? los)
  (cond
    [(empty? los) #false]
    [(cons? los) (or (= (string-length (first los)) 3)
                     (contains-three-letter-string? (rest los)))]))

(check-expect (contains-three-letter-string? EX-LOS-1) #false)
(check-expect (contains-three-letter-string? EX-LOS-2) #false)
(check-expect (contains-three-letter-string? EX-LOS-3) #true)

;; Part B

;; Design a function called hello-list that consumes an LoS and produces a new
;; LoS. The produced LoS prefixes every string in the consumed LoS with 
;; "Hello, ".

;; [TODO] Function design


;; hello-list : LoS --> LoS
;; Produces a new LoS with "Hello, " at the start of each string followed by the
;; string in the given LoS
(define (hello-list los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append "Hello, " (first los))
                       (hello-list (rest los)))]))

(check-expect (hello-list EX-LOS-1) '())
(check-expect (hello-list EX-LOS-2) (cons "Hello, susie"
                                          (cons "Hello, robert baratheon"
                                                (cons "Hello, alice" EX-LOS-1))))
(check-expect (hello-list EX-LOS-3) (cons "Hello, bob"
                                          (cons "Hello, susie"
                                                (cons "Hello, robert baratheon"
                                                      (cons "Hello, alice" EX-LOS-1)))))

