;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; This problem has a partially-completed data design for representing DNA
;; sequences. A DNA molecule is a long sequence of four nucleotides: adenine,
;; cytosine, guanine, and thymine. (More information here:
;; https://en.wikipedia.org/wiki/DNA.)

(define-struct adenine [rest])
(define-struct guanine [rest])
(define-struct cytosine [rest])
(define-struct thymine [rest])
;; A DNASeq is one of:
;; - (make-adenine DNASeq)
;; - (make-guanine DNASeq)
;; - (make-cytosine DNASeq)
;; - (make-thymine DNASeq)
;; - "empty sequence"
;; Interpretation: A DNASeq represents a sequence of nucleotides.

;; Part A

;; Write at least three examples of DNASeq. Across all examples, ensure you
;; have an example of all nucleotides.

;; [TODO] Three

(define D-0 "empty sequence")
(define D-1 (make-adenine D-0))
(define D-2 (make-guanine D-1))
(define D-3 (make-cytosine D-2))
(define D-4 (make-thymine D-3))



;; Part B

;; Write a template for DNASeq.

;; [TODO] Template

;;DNASeq-temp: DNASeq -> ???
(define (DNASeq-temp d)
  (cond
    [(and (string? d) (string=? "empty sequence" d)) ...]
    [(adenine? d) (... (DNASeq-temp (adenine-rest d)) ...)]
    [(guanine? d) (... (DNASeq-temp (guanine-rest d)) ...)]
    [(cytosine? d) (... (DNASeq-temp (cytosine-rest d)) ...)]
    [(thymine? d) (... (DNASeq-temp (thymine-rest d)) ...)]))


;; Part C

;; Every  DNA sequence has a complementary sequence, which substitutes
;; As with Ts, Ts with As, Cs with Gs, and Gs with Cs. Design a function
;; to calculate the complement of a DNA sequence.

;; [TODO] Function design recipe

;; complement: DNASeq -> DNASeq
;; It takes in a DNA sequence and replaces it with its complement
(define (complement d)
  (cond
    [(and (string? d) (string=? "empty sequence" d)) d]
    [(adenine? d) (make-thymine (complement (adenine-rest d)))]
    [(guanine? d) (make-cytosine (complement (guanine-rest d)))]
    [(cytosine? d) (make-guanine (complement (cytosine-rest d)))]
    [(thymine? d) (make-adenine (complement (thymine-rest d)))]))

(check-expect (complement D-0) D-0)
(check-expect (complement D-1) (make-thymine D-0))
(check-expect (complement D-4) (make-adenine (make-guanine (make-cytosine (make-thymine D-0)))))

