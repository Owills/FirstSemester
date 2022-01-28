;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; Part A

;; The VIA Rail Canadian is a sleeper train that runs from Vancouver to Toronto.
;; It is also featured on the Canadian $10 bill. Here is a brochure:
;;
;; https://www.viarail.ca/sites/all/files/media/pdfs/111139179-1-Sales-tools_Anglais_WEB.pdf
;;
;; Look up the list of stops the Canadian makes, and write a data definition
;; called CanadianTrainStop that can represent any of these stops.
;; Ensure you follow _all_ steps of the data design recipe.

;; [TODO] Data design recipe
;; A CanadianTrainStop is one of:
;; "Vancouver"
;; "Kamloops"
;; "Jasper"
;; "Edmonton"
;; "Saskatoon"
;; "Winnipeg"
;; "Sioux Lookout"
;; "Sudbury JCT."
;; "Toronto"
;; Interpretation: A stop of the Via Rail Canadian from Vancouver To Toronto

(define EX-VANCOUVER "Vancouver")
(define EX-KAMLOOPS "Kamloops")
(define EX-JASPER "Jasper")
(define EX-EDMONTON "Edmonton")
(define EX-SASKATOON "Saskatoon")
(define EX-WINNIPEG "Winnipeg")
(define EX-SIOUX-LOOKOUT "Sioux Lookout")
(define EX-SUDBURY-JCT. "Sudbury JCT.")
(define EX-TORONTO "Toronto")

(define (rail-canadian-temlpate cts)
  (cond
    [(string=? cts EX-VANCOUVER) ...]
    [(string=? cts EX-KAMLOOPS) ...]
    [(string=? cts EX-JASPER) ...]
    [(string=? cts EX-EDMONTON) ...]
    [(string=? cts EX-SASKATOON) ...]
    [(string=? cts EX-WINNIPEG) ...]
    [(string=? cts EX-SIOUX-LOOKOUT) ...]
    [(string=? cts EX-SUDBURY-JCT.) ...]
    [(string=? cts EX-TORONTO) ...]))

;; Part B

;; Design a function called province to determine the province in
;; which a stop is located. Ensure that you *strictly* follow the design
;; recipe.

;; [TODO] Function design recipe

;; A CanadianProvince is one of:
;; "British Columbia"
;; "Alberta"
;; "Saskat-Chewan"
;; "Manitoba"
;; "Ontario"
;; Interpretation: A large region of land in Canada

(define EX-BRITISH-COLOMBIA "British Columbia")
(define EX-ALBERTA "ALberta")
(define EX-SASKAT-CHEWAN "Saskat-Chewan")
(define EX-MANITOBA "Manitoba")
(define EX-ONTARIO "Ontario")

;; province ; CanadianTrainStop -> CanadianProvince
;; Returns what CanadianProvince the inputted CanadianTrainStop is located in

;; given "Vancouver", expect "British Colombia"
;; given "Kamloops", expect "British Colombia"
;; given "Toronto", expect "Ontario"
(define (province cts)
  (cond
    [(string=? cts EX-VANCOUVER) EX-BRITISH-COLOMBIA]
    [(string=? cts EX-KAMLOOPS) EX-BRITISH-COLOMBIA]
    [(string=? cts EX-JASPER) EX-ALBERTA]
    [(string=? cts EX-EDMONTON) EX-ALBERTA]
    [(string=? cts EX-SASKATOON) EX-SASKAT-CHEWAN]
    [(string=? cts EX-WINNIPEG) EX-MANITOBA]
    [(string=? cts EX-SIOUX-LOOKOUT) EX-ONTARIO]
    [(string=? cts EX-SUDBURY-JCT.) EX-ONTARIO]
    [(string=? cts EX-TORONTO) EX-ONTARIO]))

(check-expect (province EX-VANCOUVER) EX-BRITISH-COLOMBIA)
(check-expect (province EX-KAMLOOPS) EX-BRITISH-COLOMBIA)
(check-expect (province EX-TORONTO) EX-ONTARIO)

;; Part C

;; Some stops on the Canadian connect to other train lines. Write a predicate
;; called can-transfer? that produces #true when a stop has an available
;; transfer to another line. You must follow all steps of the design recipe.
;; However, you may shorten your function definition if you find it convenient
;; to do so.

;; [TODO] Function design recipe
;; can-transfer? ; CanadianTrainStop -> Boolean
;; Returns true if a CanadianTrainStop has a transfer to another line,
;; and false if there are no possible transfers

;; given "Vancouver", expect False
;; given "Jasper", expect True
;; given "Winnipeg", expect True

(define (can-transfer? cts)
  (cond
    [(string=? cts EX-VANCOUVER) #f]
    [(string=? cts EX-KAMLOOPS) #f]
    [(string=? cts EX-JASPER) #t]
    [(string=? cts EX-EDMONTON) #f]
    [(string=? cts EX-SASKATOON) #f]
    [(string=? cts EX-WINNIPEG) #t]
    [(string=? cts EX-SIOUX-LOOKOUT) #f]
    [(string=? cts EX-SUDBURY-JCT.) #f]
    [(string=? cts EX-TORONTO) #t]))

(check-expect (can-transfer? EX-VANCOUVER) #f)
(check-expect (can-transfer? EX-JASPER) #t)
(check-expect (can-transfer? EX-WINNIPEG) #t)