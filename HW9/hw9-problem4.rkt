;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Consider the following data definitions:
(define-struct student [name nuid])
;; A Student is a (make-student String Number)
;; Interpretation: (make-student name nuid) represents a student.

(define EX-STUDENT-1 (make-student "Alice" 1))
(define EX-STUDENT-2 (make-student "Bob" 2))
(define EX-STUDENT-3 (make-student "Carol" 3))

(define (student-template s)
  (... (student-name s) ... (student-nuid s) ...))

(define-struct grade [nuid course value])
;; A Grade is a (make-grade Number String Number)
;; Interpretation: (make-grade nuid course grade) represents the grade that
;; a student received in a course.

(define (grade-template g)
  (... (grade-nuid g) ... (grade-course g) ... (grade-value g) ...))

(define EX-GRADE-1 (make-grade 1 "Fundies 1" 95))
(define EX-GRADE-2 (make-grade 1 "Psychoceramics" 65))
(define EX-GRADE-3 (make-grade 2 "Programming Languages" 85))
(define EX-GRADE-4 (make-grade 2 "Fundies 1" 75))
(define EX-GRADE-5 (make-grade 3 "Fundies 1" 68))
(define EX-GRADE-6 (make-grade 3 "Cybernetics" 82))
(define EX-GRADE-7 (make-grade 3 "Phonology" 89))
(define EX-GRADE-8 (make-grade 4 "Fundies 1" 55))

(define-struct student-grades [name grades])
;; A StudentGrades is a (make-student-grades String [List-of Number]).
;; Interpretation: (make-student-grades name grades) represents the grades
;; that a student has received in all courses.

(define (student-grades-template sg)
  (... (student-grades-name sg) ... 
       ; i.e., template for [List-of Number]
       (lon-template (student-grades-grades sg)) ...))

(define EX-STUDENT-GRADES-1 (make-student-grades "Alice" (list 95 65)))
(define EX-STUDENT-GRADES-2 (make-student-grades "Bob" (list 85 75)))
(define EX-STUDENT-GRADES-3 (make-student-grades "Carol" (list 68 82 89)))

;; Design a function called students->student-grades that receives a list of 
;; Students and list of Grades, and produces a list of StudentGrades, where each
;; StudentGrade in the result is the list of all the grades received by a
;; student in all courses takes by that student.

;; NOTE: The list produced by students->student-grades should have an item for
;; every student in the input list, even if there are no grades for that 
;; student.

;; students->student-grades : [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;; produces a list of StudentGrades for each student in the given lists
(check-expect (students->student-grades (list EX-STUDENT-1 EX-STUDENT-2 EX-STUDENT-3)
                                        (list EX-GRADE-1 EX-GRADE-2 EX-GRADE-3 EX-GRADE-4
                                              EX-GRADE-5 EX-GRADE-6 EX-GRADE-7 EX-GRADE-8))
              (list EX-STUDENT-GRADES-1 EX-STUDENT-GRADES-2 EX-STUDENT-GRADES-3))

(check-expect (students->student-grades (list EX-STUDENT-1 EX-STUDENT-2 EX-STUDENT-3) '())
              (list (make-student-grades "Alice" '())
                    (make-student-grades "Bob" '())
                    (make-student-grades "Carol" '())))

(check-expect (students->student-grades '() (list EX-GRADE-1 EX-GRADE-2)) '())
                                        
(define (students->student-grades los log)
  (cond
    [(empty? los) '()]
    [(cons? los)     
     (cons (make-student-grades
            (student-name (first los))
            (map grade-value (filter (λ (g) (= (grade-nuid g) (student-nuid (first los)))) log)))
           (students->student-grades (rest los) log))]))


