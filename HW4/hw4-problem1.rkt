;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the following structure definitions:


;; Part A

;; Design four data types called Guitar, DrumKit, Saxophone, and Piano: one for 
;; each structure. Ensure you complete all steps of the data design recipe for 
;; all four data types.

;; [TODO] Four complete data designs

(define-struct guitar [brand-name color electric?])
;; A Guitar is a (make-guitar String String Boolean)
;; Interpretation: A (make-guitar String String Boolean) represents a
;; guitar where:
;; - brand-name is the name of the brand that made the guitar
;; - color is the color of the guitar
;; - electric? is true if the guitar is electric

(define G-1 (make-guitar "Gibson" "Black" #true))
(define G-2 (make-guitar "Heritage" "White" #false))
(define G-3 (make-guitar "Epiphone" "Purple" #true))

;; guitar-temp : Guitar -> ???
(define (guitar-temp g)
  (... (guitar-brand-name g) ...
       (guitar-color g) ...
       (guitar-electric? g) ...))


(define-struct drum-kit [brand-name electric?])
;; A Drum-kit is a (make-drum-kit String Boolean)
;; Interpretation: A (make-drum-kit String Boolean) represents a
;; drum where:
;; - brand-name is the name of the brand that made the drum
;; - electric? is true if the drum is electric

(define D-1 (make-drum-kit "Tama" #true))
(define D-2 (make-drum-kit "Yamaha" #false))
(define D-3 (make-drum-kit "Ludwig" #true))

;; drum-temp : Drum-kit -> ???
(define (drum-temp d)
  (... (drum-kit-brand-name d) ...
       (drum-kit-electronic? d) ...))


(define-struct saxophone [brand-name])
;; A Saxophone is a (make-saxophone String)
;; Interpretation: A (make-saxophone String) represents a
;; saxophone where:
;; - brand-name is the name of the brand that made the saxophone

(define S-1 (make-saxophone "Yamaha"))
(define S-2 (make-saxophone "Selmer Paris"))
(define S-3 (make-saxophone "Yanagisawa"))

;; sax-temp: Saxophone -> ???
(define (sax-temp s)
  (... (saxophone-brand-name s) ...))


(define-struct piano [brand-name])
;; A Piano is a (make-piano String)
;; Interpretation: A (make-piano String) represents a
;; piano where:
;; - brand-name is the name of the brand that made the piana

(define P-1 (make-piano "Yamaha"))
(define P-2 (make-piano "Sauter"))
(define P-3 (make-piano "Grotrian"))

;; piano-temp: Piano -> ???
(define (piano-temp p)
  (... (piano-brand-name p) ...))




;; Part B

;; Design a data type called Instrument, which can represent any one of the
;; four instruments defined above.

;; [TODO] Data design recipe

;; An Instrument is one of:
;; - Piano
;; - Saxophone
;; - Drum-kit
;; - Guitar
;; Interpretation: A musical instrument

(define I-1 P-1)
(define I-2 S-1)
(define I-3 D-1)
(define I-4 G-1)

;; instrument-temp: Instrument -> ???
(define (instrument-temp i)
  (...
   (cond
     [(piano? i) ...]
     [(saxophone? i) ...]
     [(drum-kit? i) ...]
     [(guitar? i) ...])))


;; Part C

;; Design a data type called Band, which may have 1, 2, or 3 instruments.
;; The Band data type should hold information about all the instruments in
;; the band.

;; [TODO] Data design recipe

(define-struct band [inst-1 inst-2 inst-3])
;; A Band is one of:
;; - (make-band Instrument String String)
;; - (make-band Instrument Instrument String)
;; - (make-band Instrument Instrument Instrument)
;; - Interpretation: A (make-band Instrument String String) represents
;; a band with one instrument where:
;; - inst-1 is Instrument
;; - inst-2 is "none"
;; - inst-3 is "none"
;; - Interpretation: A (make-band Instrument Instrument String) represents
;; a band with two instruments where:
;; - inst-1 is Instrument
;; - inst-2 is Instrument
;; - inst-3 is "none"
;; - Interpretation: A (make-band Instrument Instrument Instrument) represents
;; a band with three instruments where:
;; - inst-1 is Instrument
;; - inst-2 is Instrument
;; - inst-3 is Instrument

(define B-1 (make-band G-1 "none" "none"))
(define B-2 (make-band G-2 P-1 "none"))
(define B-3 (make-band G-3 D-2 S-1))

(define B-4 (make-band S-1 G-2 D-2))
(define B-5 (make-band S-1 D-2 G-2))
(define B-6 (make-band D-2 S-2 P-1))

;; band-temp: Band -> ???
(define (band-temp b)
  (... (band-inst-1 b) ...
       (band-inst-2 b) ...
       (band-inst-3 b) ...))


;; Part D

;; Design a function that takes a band and produces another band that is
;; identical, except that all guitars and drums become electric!

;; [TODO] Function design

;; make-electric: Band -> Band
;; Changes the guitars and drums in a band to electric
(define (make-electric b)
  (cond
    [(and (guitar? (band-inst-1 b)) (not (guitar-electric? (band-inst-1 b))))
     (make-electric (make-band (make-guitar (guitar-brand-name (band-inst-1 b))
                                            (guitar-color (band-inst-1 b))
                                            #true)
                               (band-inst-2 b) (band-inst-3 b)))]
    [(and (drum-kit? (band-inst-1 b)) (not (drum-kit-electric? (band-inst-1 b))))
     (make-electric (make-band (make-drum-kit (drum-kit-brand-name (band-inst-1 b))
                                              #true)
                               (band-inst-2 b) (band-inst-3 b)))]
    [(and (guitar? (band-inst-2 b)) (not (guitar-electric? (band-inst-2 b))))
     (make-electric  (make-band (band-inst-1 b)
                                (make-guitar (guitar-brand-name (band-inst-2 b))
                                             (guitar-color (band-inst-2 b))
                                             #true)
                                (band-inst-3 b)))]
    [(and (drum-kit? (band-inst-2 b)) (not (drum-kit-electric? (band-inst-2 b))))
     (make-electric (make-band (band-inst-1 b)
                               (make-drum-kit (drum-kit-brand-name (band-inst-2 b))
                                              #true)
                               (band-inst-3 b)))]
    [(and (guitar? (band-inst-3 b)) (not (guitar-electric? (band-inst-3 b))))
     (make-electric (make-band (band-inst-1 b)
                               (band-inst-2 b)
                               (make-guitar (guitar-brand-name (band-inst-3 b))
                                            (guitar-color (band-inst-3 b))
                                            #true)))]
    [(and (drum-kit? (band-inst-3 b)) (not (drum-kit-electric? (band-inst-3 b))))
     (make-electric (make-band (band-inst-1 b)
                               (band-inst-2 b)
                               (make-drum-kit (drum-kit-brand-name (band-inst-3 b))
                                              #true)))]
    [else b]))

(check-expect (make-electric B-1) B-1)
(check-expect (make-electric B-2) (make-band (make-guitar "Heritage" "White" #true) P-1 "none"))
(check-expect (make-electric B-3) (make-band G-3 (make-drum-kit "Yamaha" #true) S-1))
(check-expect (make-electric B-4) (make-band S-1 (make-guitar "Heritage" "White" #true)
                                             (make-drum-kit "Yamaha" #true)))
(check-expect (make-electric B-5) (make-band S-1 (make-drum-kit "Yamaha" #true)
                                             (make-guitar "Heritage" "White" #true)))
(check-expect (make-electric B-6) (make-band (make-drum-kit "Yamaha" #true)
                                             S-2 P-1))
