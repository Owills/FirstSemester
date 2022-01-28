;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; In most organizations, employeers are arranged in a hierarchy called an
;; "organization chart" or org-chart. For example, here is a portion of the 
;; org-chart for Northeastern:
;;
;;                      Joseph E. Aoun
;;                      (President)
;;                           |
;;                      The Cabinet                                   
;;                           |
;;       +-------------------+-------------------+
;;   Karl Reid      Madeleine Estabrook    David Madigan
;; (Chief Inclusion  (Student Affairs)    (Academic Affairs)
;;     Officer)                                 |
;;                    +-------------------------+---------+
;;                    |                                   |
;;              Administration                      Academic Deans
;;                    |                                   |
;;                    |                  +----------------+---------------+
;;                    |                  |                |               | 
;;              Thomas Sheahan      Alan Mislove    Carmen Sceppa    Uta Poiger
;;         (Curriculum & Programs)    (Khoury)         (Bouve)     (Social Sciences &
;;                                                                      Humanities)
;;
;; The people in this chart have a name (obviously) and a title. Each also
;; has a number of "direct reports" (possible no direct reports). In addition,
;; each set of direct reports is grouped together with a label. E.g., the three
;; groups above are "The Cabinet", "Administration", and "Academic Affairs".
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A
;;
;; Design data called OrgChart to represent an org-chart.
;; One of your examples must have *all* the information in the org-chart picture
;; shown above.

;; [TODO] Data design

(define-struct label [group people])
; A label is one of (make-direct-report String [List-Of OrgChart])
; Interpretation: a group of people in an Orgchart that share a common label, where
; group is one of "The Cabinet", "Administration", and "Academic Affairs",
; and people is a list of Orgchart that represent the people in that group

(define-struct org-chart [name title direct-reports])
; An OrgChart is a (make-org-chart String String [List-of Label])
; Interpretation: an organization chart for Northeastern, where
; name is the name of the person,
; title is the title of their position,
; and direct-reports represent the people in the heirarchy below them 

(define OC-1 (make-org-chart "Alan Mislove" "Khoury" '()))
(define OC-2 (make-org-chart "Carmen Sceppa" "Bouve" '()))
(define OC-3 (make-org-chart "Uta Poiger" "Social Sciences & Humanities" '()))
(define OC-4 (make-org-chart "Thomas Sheahan" "Curriculum & Programs" '()))

(define L-1 (make-label "Academic Deans" (list OC-1 OC-2 OC-3)))
(define L-2 (make-label "Administration" (list OC-4)))

(define OC-5 (make-org-chart "David Madigan" "Academic Affairs" (list L-1 L-2)))
(define OC-6 (make-org-chart "Madeleine Estabrook" "Student Affairs" '()))
(define OC-7 (make-org-chart "Karl Reid" "Chief Inclusion Officer" '()))

(define L-3 (make-label "Cabinet" (list OC-5 OC-6 OC-7)))

(define OC-8 (make-org-chart " Joseph E. Aoun" "President" (list L-3)))


(define (org-chart-temp oc)
  (... (org-chart-name oc) ...
       (org-chart-title oc) ...
       (lol-temp (org-chart-direct-reports oc)) ...))

(define (label-temp l)
  (... (label-group l) ...
       (looc-temp (label-people l)) ...))

(define (lol-temp lol)
  (...
   (cond
     [(empty? lol) ...]
     [(cons? lol) ...
      (label-temp (first lol)) ...
      (lol-temp (rest lol)) ...])))

(define (looc-temp looc)
  (...
   (cond
     [(empty? looc) ...]
     [(cons? looc) ...
      (org-chart-temp (first looc)) ...
      (looc-temp (rest looc)) ...])))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function num-peeps that counts how many people are in an OrgChart.

;; [TODO] Function design

;; num-peeps : OrgChart -> Nat
;; counts how many people are in an OrgChart

(check-expect (num-peeps OC-1) 1)
(check-expect (num-peeps OC-8) 8)

(define (num-peeps oc)
  (+ 1 (lol-counter (org-chart-direct-reports oc))))

;; lol-counter : [List-of Label] -> Nat
;; counts how many people are in a List-of Label

(check-expect (lol-counter (list L-1 L-2)) 4)
(check-expect (lol-counter (list L-3)) 7)

(define (lol-counter lol)
  (cond
    [(empty? lol) 0]
    [(cons? lol) 
     (+ (looc-counter (label-people (first lol)))
        (lol-counter (rest lol)))]))

;; counter : [List-of OrgChart] -> Nat
;; counts how many people are in an List-of OrgChart

(check-expect (looc-counter (list OC-1 OC-2 OC-3 OC-4)) 4)
(check-expect (looc-counter (list OC-5 OC-6 OC-7)) 7)

(define (looc-counter looc)
  (cond
    [(empty? looc) 0]
    [(cons? looc) (+
                   (num-peeps (first looc)) 
                   (looc-counter (rest looc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function full-title that consumes an OrgChart and the name of
;; an organization, and produces an OrgChart that adds the "(organization name)"
;; to the title for each person in the OrgChart.

;; full-title : OrgChart String -> OrgChart
;; produces an OrgChart that adds the "(organization name)"
;; to the title for each person in the OrgChart


(check-expect (full-title OC-1 "Northeastern")
              (make-org-chart "Alan Mislove" "Khoury (Northeastern)" '()))

(check-expect (full-title OC-5 "Northeastern")
              (make-org-chart
               "David Madigan" "Academic Affairs (Northeastern)"
               (list
                (make-label "Academic Deans"
                            (list
                             (make-org-chart "Alan Mislove" "Khoury (Northeastern)" '())
                             (make-org-chart "Carmen Sceppa" "Bouve (Northeastern)" '())
                             (make-org-chart "Uta Poiger"
                                             "Social Sciences & Humanities (Northeastern)" '())))
                (make-label "Administration"
                            (list
                             (make-org-chart "Thomas Sheahan"
                                             "Curriculum & Programs (Northeastern)" '()))))))


(define (full-title oc organization)
  (make-org-chart (org-chart-name oc)
                  (string-append (org-chart-title oc) " (" organization ")")
                  (full-title-labels (org-chart-direct-reports oc) organization)))

;; full-title-labels : [List-of Label] -> [List-of Label]
;; produces an List-of Label that adds the "(organization name)"
;; to the title for each person in the that is represented in that Label

(check-expect (full-title-labels '() "Northeastern") '())
(check-expect (full-title-labels (list L-1 L-2) "Northeastern")
              (list
               (make-label "Academic Deans"
                           (list
                            (make-org-chart "Alan Mislove" "Khoury (Northeastern)" '())
                            (make-org-chart "Carmen Sceppa" "Bouve (Northeastern)" '())
                            (make-org-chart "Uta Poiger"
                                            "Social Sciences & Humanities (Northeastern)" '())))
               (make-label "Administration"
                           (list
                            (make-org-chart "Thomas Sheahan"
                                            "Curriculum & Programs (Northeastern)" '())))))

(define (full-title-labels lol organization)
  (cond
    [(empty? lol) '()]
    [(cons? lol) 
     (cons (make-label (label-group (first lol))
                       (full-title-oc (label-people (first lol)) organization))
           (full-title-labels (rest lol) organization))]))

;; full-title-oc : [List-of OrgChart] -> [List-of OrgChart]
;; produces an List-of OrgChart that adds the "(organization name)"
;; to the title for each person in the List-of OrgChart

(check-expect (full-title-oc '() "Northeastern") '())
(check-expect (full-title-oc (list OC-1 OC-2 OC-3) "Northeastern")
              (list
               (make-org-chart "Alan Mislove" "Khoury (Northeastern)" '())
               (make-org-chart "Carmen Sceppa" "Bouve (Northeastern)" '())
               (make-org-chart "Uta Poiger" "Social Sciences & Humanities (Northeastern)" '())))

(define (full-title-oc looc organization)
  (cond
    [(empty? looc) '()]
    [(cons? looc) (cons
                   (full-title (first looc) organization) 
                   (full-title-oc (rest looc) organization))]))

