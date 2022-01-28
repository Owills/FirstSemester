;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw7-all-problems) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All Problems for Homework 7

;; This assignment asks you to design several functions that employ the
;; following data designs. The functions that you design *must* use list
;; abstraction(s) when appropriate.
;;
;; NOTE #1: Part of the credit for each problem will be based on the choice of
;; list abstractions, so make sure that they are a good match for the problem.
;;
;; NOTE #2: For certain problems, you will have to design helper functions that
;; do not use list abstractions. You should follow the full design recipe
;; (including templates) for all problems.
;;
;; NOTE #3: For every function that you design, follow the "2+ tests and no
;; halloween colors" rule.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Designs (do not modify these)

;; A CallType is one of:
;; - "zoom"
;; - "teams"
;; - "phone"
;; Interpretation: a type of call

(define CT-ZOOM "zoom")
(define CT-TEAMS "teams")
(define CT-PHONE "phone")

(define (calltype-temp ct)
  (cond
    [(string=? ct CT-ZOOM) ...]
    [(string=? ct CT-TEAMS) ...]
    [(string=? ct CT-PHONE) ...]))

(define-struct call [type duration attendees description])
(define-struct mtg [duration attendees description])
(define-struct alone [duration description])

;; An Event is one of:
;; - (make-call CallType PosInt [NEList-of String] String)
;; - (make-mtg PosInt [NEList-of String] String)
;; - (make-alone PosInt String)
;; Interpretation: an event in some period of time, which is either:
;; - A call using some technology, lasting some number of minutes with attendees
;;  (by name), and a description;
;; - An in-person meeting lasting some number of minutes
;;   with attendees (by name) and a description; or
;; - Time spent alone for some number of minutes with a description.

(define E-ZOOM-DOC
  (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :("))
(define E-TEAMS-OH
  (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!"))
(define E-PHONE-SPAM
  (make-call CT-PHONE 1 (list "Unknown")
             "Who calls!? I think it was a scam..."))
;; These are characters from a TV show called "Friends", which was popular in
;; the 90s, which is when many of your instructors grew up.
(define E-MTG-STUDY
  (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!"))
(define E-MTG-ADVISOR
  (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major"))
(define E-ALONE-LUNCH
  (make-alone 34 "Lunch"))
(define E-ALONE-READING
  (make-alone 25 "The Three-Body Problem isn't going to read itself!"))
(define LOE-1
  (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM
        E-ALONE-LUNCH E-TEAMS-OH E-MTG-ADVISOR E-MTG-STUDY))


(define (e-temp e)
  (cond
    [(call? e)
     (... (calltype-temp (call-type e)) ...
          (call-duration e) ...
          (los-temp (call-attendees e)) ...
          (call-description e) ...)]
    [(mtg? e)
     (... (mtg-duration e) ...
          (los-temp (mtg-attendees e)) ...
          (mtg-description e) ...)]
    [(alone? e)
     (... (alone-duration e) ...
          (alone-description e) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design a function called weekend that reminds you to rest. The function
;; consumes an argument -- a positive integer -- and produces a list of that
;; many alone events. Each of these is 30mins each, with the description "rest".

;; [TODO] Function design. Use list abstractions when appropriate.

(define ALONE-REST
  (make-alone 30 "rest"))

;; weekend : Integer -> [List-of Event]
;; Creates a list of a certain number of rest periods

(check-expect (weekend 0) '())
(check-expect (weekend 5) (list ALONE-REST ALONE-REST ALONE-REST ALONE-REST ALONE-REST))

(define (weekend w)
  (build-list w weekend-help))

;; weekend-help : Integer -> Alone
;; Creates a rest period
(define (weekend-help w)
  (make-alone 30 "rest"))

(check-expect (weekend-help 0) ALONE-REST)
(check-expect (weekend-help 5) ALONE-REST)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function small-group that consumes a list of events and only
;; produces those that have fewer than three participants. In all cases,
;; there is an implied attendee (you!) that counts as one person. So,
;; calls and meetings with two or more attendees are *not* small groups.

;; [TODO] Function design. Use list abstractions when appropriate.


;; small-group : [List-of Event] -> [List-of Event]
;; Produces a list where each event has less than 3 participants

(check-expect (small-group '()) '())
(check-expect (small-group LOE-1) (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM
                                        E-ALONE-LUNCH E-MTG-ADVISOR))
(define (small-group loe)
  (filter small-helper loe))

;; small-helper : Event -> Boolean
;; Returns true if the event has less than 3 participants

(check-expect (small-helper E-ALONE-LUNCH) #true)
(check-expect (small-helper E-MTG-ADVISOR) #true)
(check-expect (small-helper E-TEAMS-OH) #false)

(define (small-helper e)
  (cond
    [(call? e)
     (if (< (length (call-attendees e)) 2) #true #false)]
    [(mtg? e)
     (if (< (length (mtg-attendees e)) 2) #true #false)]
    [(alone? e)
     #true]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function had-lunch? that accepts a list of events and determines
;; if it contains an event whose description contains the word "lunch" 
;; (without regard for upper/lower-case).

;; [TODO] Function design. Use list abstractions when appropriate.


;; had-lunch? : [List-of Event] -> Boolean
;; Checks if any events in a list have the description as lunch
(check-expect (had-lunch? '()) #false)
(check-expect (had-lunch? LOE-1) #true)

(define (had-lunch? loe)
  (ormap lunch? loe))


;; lunch? : Event -> Boolean
;; Checks of an event has the description of lunch
(check-expect (lunch? E-ZOOM-DOC) #false)
(check-expect (lunch? E-MTG-STUDY) #false)
(check-expect (lunch? E-ALONE-LUNCH) #true)

(define (lunch? e)
  (cond
    [(call? e)
     (string=? "Lunch" (call-description e))]
    [(mtg? e)
     (string=? "Lunch" (mtg-description e))]
    [(alone? e)
     (string=? "Lunch" (alone-description e))]))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part D

;; Design the function social-time that accepts a list of events and produces
;; the total minutes spent on calls or meetings.

;; [TODO] Function design. Use list abstractions when appropriate.

;; social-time : [List-of Event] -> Integer
;; Adds up the duration of all the events in the list
(check-expect (social-time '()) 0)
(check-expect (social-time LOE-1) 179)

(define (social-time loe)
  (foldr + 0 (map get-duration loe)))

;; get-duration : Event -> PosInt
;; Gets the duration of an event
(check-expect (get-duration E-ZOOM-DOC) 22)
(check-expect (get-duration E-MTG-STUDY) 62)
(check-expect (get-duration E-ALONE-LUNCH) 34)

(define (get-duration e)
  (cond
    [(call? e)
     (call-duration e)]
    [(mtg? e)
     (mtg-duration e)]
    [(alone? e)
     (alone-duration e)]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Part E

;; Design the function anything-but-phone? that accepts a list of events and
;; and produces #true if none of the calls were phone calls. (Zoom and Teams
;; calls are not phone calls.)

;; [TODO] Function design. Use list abstractions when appropriate.

;; anything-but-phone? : [List-of Event] -> Boolean
;; Checks if all calls in a list of event are phone calls
(check-expect (anything-but-phone? '()) #true)
(check-expect (anything-but-phone? LOE-1) #false)

(define (anything-but-phone? loe)
  (andmap phone? (filter call? loe)))

;; phone? : CallType -> Boolean
;; Checks if a call is a phone call
(check-expect (phone? E-ZOOM-DOC) #false)
(check-expect (phone? E-PHONE-SPAM) #true)

(define (phone? c)
  (string=? CT-PHONE (call-type c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part F

;; Design the function summary that accepts a list of
;; events and produces a list of textual summaries for each; for example...
;;
;; "Had a Teams call with Mike and Tajel for 7min:
;;  'Office hours with my partner to ask clarifying questions about the Design Recipe!'"
;;
;; "Met with Ali for 28min: 'Meeting with advisor to talk about a combined major'"
;;
;; "Spent 25min: 'The Three-Body Problem isn't going to read itself!'"
;;
;; Your summaries do not have exactly the same format as the examples above. 
;; But, they should contain the same information.

;; [TODO] Function design. Use list abstractions when appropriate.

(define LOE-2 (list E-TEAMS-OH E-MTG-ADVISOR E-ALONE-READING))

;; sumarry : [List-Of Event] -> [List-Of String]
;; Makes a list of the summaries of each event
(check-expect (summary '()) '())
(check-expect (summary LOE-2)
              (list
               (string-append "Had a teams call with Mike and Tajel for 7min:"
                              " 'Office hours with my partner to ask clarifying questions"
                              " about the Design Recipe!'")
               "Met with Ali for 28min: 'Meeting with advisor to talk about a combined major'"
               "Spent 25min: 'The Three-Body Problem isn't going to read itself!'"))

(define (summary los)
  (map summary-help los))


;; summary-help : Event -> String
;; Makes a summary for an event


(check-expect (summary-help E-TEAMS-OH) (string-append "Had a teams call with Mike and"
                                                       " Tajel for 7min: 'Office hours with "
                                                       "my partner to ask clarifying"
                                                       " questions about the Design Recipe!'"))
(check-expect (summary-help E-MTG-ADVISOR) (string-append "Met with Ali for 28min: "
                                                          "'Meeting with advisor to talk about a "
                                                          "combined major'"))
(check-expect (summary-help E-ALONE-READING) (string-append "Spent 25min: "
                                                            "'The Three-Body Problem"
                                                            " isn't going to read itself!'"))

(define (summary-help e)
  (cond
    [(call? e)
     (string-append "Had a " (call-type e) " call with "
                    (attendees-helper (call-attendees e)) " for "
                    (number->string (call-duration e)) "min: '"
                    (call-description e) "'")]
    [(mtg? e)
     (string-append "Met with " (attendees-helper (mtg-attendees e))
                    " for " (number->string (mtg-duration e)) "min: '"
                    (mtg-description e) "'")]
    [(alone? e)
     (string-append "Spent " (number->string (alone-duration e)) "min: '"
                    (alone-description e) "'")]))

;; attendees-helper : [NEList-Of-String] -> String
;; Adds and in between each element of a list

(check-expect (attendees-helper (list "Mike" "Tajel")) "Mike and Tajel")
(check-expect (attendees-helper (list "Ali")) "Ali")

(define (attendees-helper los)
  (string-join los " and "))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part G

;; Design the function peeps that accepts a list of
;; events and produces an alphabetically sorted list of attendees
;; at all the calls/meetings (including any duplicates).

;; [TODO] Function design. Use list abstractions when appropriate.

;; peeps : [List-of Event] -> [List-of String]
;; Puts the attendees names in alphabetical order for each event in the list

(check-expect (peeps '()) '())
(check-expect (peeps LOE-1) (list "Ali" "Chandler" "Dr. Zoidberg" "Joey"
                                  "Mike" "Monica" "Phoebe" "Rachel" "Ross" "Tajel" "Unknown"))
(define (peeps loe)
  (sort (foldr append '() (map peeps-helper loe)) string<?))

;; peeps-helper : Event -> [List-of String]
;; Gives attendees in a list
(define (peeps-helper e)
  (cond
    [(call? e)
     (call-attendees e)]
    [(mtg? e)
     (mtg-attendees e)]
    [(alone? e) empty]))

(check-expect (peeps-helper E-ZOOM-DOC) (list "Dr. Zoidberg"))
(check-expect (peeps-helper E-MTG-ADVISOR) (list "Ali"))
(check-expect (peeps-helper E-ALONE-LUNCH) empty)
              






