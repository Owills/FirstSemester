;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; In western classical music, tones are typically placed on a scale called
;; the twelve-tone scale. We use non-negative integers to refer to each tone.
;; For example 60 refers to the tone called "C" (or the "do" in "do-re-mi")
;; near the middle of a piano, whereas 61 refers to the tone one unit higher.
;; We consider two tones with a gap of a multiple of 12 units between them as
;; equivalent. For example, the tones 0, 60 and 84 are all equivalent: they are
;; all the tone "C". However, tones 60 and 67 are not equivalent.

;; Part A

;; Define a function called tone-class which consumes a single tone as an
;; argument, and produces its *class*,  which is the smallest non-negative
;; integer that is equivalent to the tone. For example, the class of 60 is 0,
;; the class of 61 is 1, and the class of 0 is 0 itself. You must also write 
;; three examples for your function.
;;
;; Hint: Since there are 12 classes starting with zero, you can calculate the
;; class as the remainder. Try looking for relevant functions in the DrRacket
;; Help Desk.

;; [TODO] Function definition
;; tone-class ; number -> number
;; Divides the input by 12 a returns the remainder
(define (tone-class tone)
  (remainder tone 12))
  
;; [TODO] Three examples
;; (tone-class 11) ; pproduces 11
;; (tone-class 69) ; pproduces 9
;; (tone-class 58) ; pproduces 10

;; Part B

;; The distance between two tones is how far apart they are, while keeping
;; equivalence in mind. Since there are 12 tone classes, the maximum distance
;; between any pair of tones is 12. However, there are two distances you can
;; produce, depending on which tone you consider first:
;;
;; - The distance between tones 60 and 63 is either 3 (counting up) or 9
;;   (counting down).
;; - The distance between 60 and 75 is also either 3 or 9.
;; - The distance between 63 and 70 is 5 or 7.

;; Write a function called tone-distance which consumes two tones as arguments,
;; and produces their distance (either distance), as defined above.
;; Write three examples for tone-distance.

;; [TODO] Function definition

;; tone-class ; number number -> number
;; converts each number to its smallest equivelant tone using tone-class
;; and then returns the non negative difference
(define (tone-distance first-tone second-tone)
  (if (> (- (tone-class first-tone) (tone-class second-tone)) 0)
      (- (tone-class first-tone) (tone-class second-tone))
      (- (tone-class second-tone) (tone-class first-tone))))

;; [TODO] Thee examples
;; (tone-distance 60 72) ; produces 0
;; (tone-distance 11 81) ; produces 2
;; (tone-distance 15 16) ; produces 1

;; Part C

;; On a piano keyboard, the each class of twelve tones (a.k.a., an octave) are
;; placed in a standard pattern of eight white and five black keys. If you are
;; not familiar with this pattern, here is a picture of a piano keyboard:
;;
;; https://en.wikipedia.org/wiki/Musical_keyboard#/media/File:Klaviatur-3-en.svg
;;
;; Write a function called keyboard that consumes the height and width
;; of the white keys, and produces an image that looks like a piano octave.
;; The black keys are roughly half the width and about 3/4 the length of the
;; white keys.
;;
;; Note: The picture linked above labels the white keys. Your image does not
;; have to do so.
;;
;; Hint 1: The overlay/align function may be very helpful.
;;
;; Hint 2: You can use "transparent" as a color for a rectangle.

(require 2htdp/image)
(require 2htdp/universe)

;; keyboard : Number Number -> Image
;; Uses the height and width of a single white key to create and image of a full octave
(define (keyboard white-key-width white-key-height)
  (place-images
   (list (rectangle white-key-width white-key-height "outline" "black")
         (rectangle (* white-key-width 0.5) (* white-key-height 0.75) "solid" "black")
         (rectangle white-key-width white-key-height "outline" "black")
         (rectangle (* white-key-width 0.5) (* white-key-height 0.75) "solid" "black")
         (rectangle white-key-width white-key-height "outline" "black")
         (rectangle white-key-width white-key-height "outline" "black")
         (rectangle (* white-key-width 0.5) (* white-key-height 0.75) "solid" "black")
         (rectangle white-key-width white-key-height "outline" "black")
         (rectangle (* white-key-width 0.5) (* white-key-height 0.75) "solid" "black")
         (rectangle white-key-width white-key-height "outline" "black")
         (rectangle (* white-key-width 0.5) (* white-key-height 0.75) "solid" "black")
         (rectangle white-key-width white-key-height "outline" "black"))
   (list (make-posn (* white-key-width 0.5) (* white-key-height 0.5))
         (make-posn (* white-key-width 1) (* white-key-height 0.5 0.75))
         (make-posn (* white-key-width 1.5) (* white-key-height 0.5))
         (make-posn (* white-key-width 2) (* white-key-height 0.5 0.75))
         (make-posn (* white-key-width 2.5) (* white-key-height 0.5))
         (make-posn (* white-key-width 3.5) (* white-key-height 0.5))
         (make-posn (* white-key-width 4) (* white-key-height 0.5 0.75))
         (make-posn (* white-key-width 4.5) (* white-key-height 0.5))
         (make-posn (* white-key-width 5) (* white-key-height 0.5 0.75))
         (make-posn (* white-key-width 5.5) (* white-key-height 0.5))
         (make-posn (* white-key-width 6) (* white-key-height 0.5 0.75))
         (make-posn (* white-key-width 6.5) (* white-key-height 0.5)))
   (rectangle (* white-key-width 7.1) (+ white-key-height +1) "outline" "transparent")))
  
 
  


