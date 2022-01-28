;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-all-problems) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; Spelling Bee is a vocabulary game hosted by the New York Times. The project
;; for this class is to build a program similar to Spelling Bee over the course
;; of several homework assignments. Before reading any further, you should play
;; the New York Times' Spelling Bee to understand the rules of the game:
;;
;; https://www.nytimes.com/puzzles/spelling-bee
;;
;; Your first version of Spelling Bee, which you will design for this
;; assignment, will make several simplifications:
;;
;; 1. You will construct words using five letters, and not seven,
;; 2. You will be able to enter nonsensical words,
;; 3. You will not be able to correct mistakes (i.e., backspace will not
;;      work),
;; 4. You will be able enter the same word several times, and
;; 5. You won't keep score.
;;
;; In later homework, you will remove these restrictions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Representing and Displaying Available Letters
;;

;; Design data called Letters that holds the five letters that are
;; available for your simplified Spelling Bee. Note that one of the letters is
;; distinguished as the required letter (i.e., the letter displayed at the
;; center).
;;
;; Tip: Use "1String" to refer to a one-character string in your data 
;; definition.

;; [TODO] Data design recipe

;; A Letter is a:
;; "a"
;; "b"
;; "c"
;; "d"
;; "e"
;; Intepreation: A single letter in the English alphabet

(define-struct letters [main letter1 letter2 letter3 letter4])
;; A Letters is a (make-letters Letter Letter Letter Letter Letter)

;; Interpretation: A (make-letters Letter Letter Letter Letter Letter) represents the
;; five letters available for this simplified spelling bee:
;; - main-letter is the required letter at the center of the display
;; - letter1 is a non required letter
;; - letter2 is a non required letter
;; - letter3 is a non required letter
;; - letter4 is a non required letter

(define letters-set-1 (make-letters "a" "b" "c" "d" "e"))
  
;; letters-template : Letters -> ???
(define (letters-template l)
  (... (letters-main l) ...
       (letters-letter1 l) ...
       (letters-letter2 l) ...
       (letters-letter3 l) ...
       (letters-letter4 l) ...))


;; Design a function to display Letters as an Image. Feel free to layout the
;; letters in any way you like. However, the required letter must be
;; distinguished in some way (e.g., by position, color, font, etc).
;;
;; NOTE: You must not use substring in this function, or in any helper function
;; that it employs.

;; [TODO] Function design recipe

;; draw-letters: letters -> Image
;; draws letters, with the required letter highlighed in the center and the other letters surounding
(define (draw-letters l)
  (place-images
   (list (text (letters-main l) 24 "black")
         (text (letters-letter1 l) 24 "black")
         (text (letters-letter2 l) 24 "black")
         (text (letters-letter3 l) 24 "black")
         (text (letters-letter4 l) 24 "black")
         (pulled-regular-polygon 40 4 1/3 30 "solid" "blue"))
   (list (make-posn 80 100)
         (make-posn 110 100)
         (make-posn 50 100)
         (make-posn 80 70)
         (make-posn 80 130)
         (make-posn 80 100))
   (rectangle 300 300 "solid" "goldenrod")))

;(draw-letters letters-set-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: Representing the World

;; A game of Spelling Bee must track the available letters and the partial word
;; that the player has entered. Design data called World that holds
;; this data, and design a function called world->image that displays a World as
;; an image. You can produce any image that you like, but it should clearly show
;; both the available letters and the partial word.

;; Note: The final step of this homework has you revise the data definition for
;; World. We recommend completing this step before the revision. However, we
;; recommend you read the whole assignment first so that you can understand
;; the revision that you will have to do.

;; [TODO] Data design recipe
(define-struct world [available-letters partial-word])
;; A World is a (make-world Letters String)

;; Interpretation: A (make-world Letters String) represents the
;; the avaiable letters a user to can use to make a word, and the partial word the user has entered:
;; available-letters is the possible Letters the user can use
;; partial-word is the series of letters the user has entered

(define world-1 (make-world letters-set-1 "BEAD"))
(define world-2 (make-world letters-set-1 ""))

;; world-template : World -> ???
(define (world-template w)
  (... (world-available-letters w) ...
       (world-partial-word w) ...))

;; [TODO] Function design recipe

;; world->image : World -> Image
;; Displays the information in World as an Image
(define (world->image w)
  (place-image
   (text (world-partial-word w) 12 "black")
   180 100
   (draw-letters (world-available-letters w))))

;;(world->image world-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3:  Player Interactions

;; In a game of Spelling Bee, the player can either enter a letter to add to the
;; current word, or press the "Enter" key to complete the word. Design a
;; function with the following signature:

;; key-pressed : World KeyEvent -> World

;; [TODO] Complete the function design recipe

;; key-pressed : World KeyEvent -> World
;; updates the partial-word information in the World
(define (key-pressed w key)
  (cond
    [(string=? key (letters-main (world-available-letters w)))
     (make-world (world-available-letters w)
                 (string-append (world-partial-word w) (letters-main (world-available-letters w))))]
    [(string=? key (letters-letter1 (world-available-letters w)))
     (make-world (world-available-letters w)
                 (string-append (world-partial-word w)
                                (letters-letter1 (world-available-letters w))))]
    [(string=? key (letters-letter2 (world-available-letters w)))
     (make-world (world-available-letters w)
                 (string-append (world-partial-word w)
                                (letters-letter2 (world-available-letters w))))]
    [(string=? key (letters-letter3 (world-available-letters w)))
     (make-world (world-available-letters w)
                 (string-append (world-partial-word w)
                                (letters-letter3 (world-available-letters w))))]
    [(string=? key (letters-letter4 (world-available-letters w)))
     (make-world (world-available-letters w)
                 (string-append (world-partial-word w)
                                (letters-letter4 (world-available-letters w))))]
    [(and (string=? key "\r")
          (string-contains? (letters-main (world-available-letters w)) (world-partial-word w)))
     (make-world (world-available-letters w) "")]
    [else w]))

;; When the key is an an available letter, key-pressed should produce a new
;; world that adds that letter to the end of the current word. If the key is not
;; an available letter, it should produce the previous world. If the key is the
;; special string "\r" (which stands for the Enter key), it should produce a new
;; world with the empty string for the current word, as long as the current word
;; contains the center letter. If the player presses any other key, produce the
;; previous world unchanged. In other words, your program should not produce an
;; error if the player presses the "wrong" key.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 4: World Program

;; At this point, you have enough in place to play a basic game. Use the
;; following world program to play spelling bee.

;; play : World -> World
;;
;; Uses big-bang to play a game of Spelling Bee, given Letters.
(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))

;; [TODO] Click Run. Then, in Interactions, use the function play on an 
;; example of Letters.

;(play world-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Keeping Track of Words

;; The Spelling Bee program that you have so far doesn't even keep track of the
;; words you've entered! We really need the game to show the words that
;; you've already entered. For example, if you previously entered the words
;; "bell" and "well", and the available letters are "b", "o", "w", "e", and "l",
;; with "l" at the center, the program should really display something that
;; looks like this:
;;
;;   O     Words so far:
;; B L W   bell
;;   E     well
;;
;; Here is how you can display text on multiple lines. In a string, the special
;; character "\n" represents a new line. So, the string
;; "Words so far:\nbell\nwell" represents a string that appears on three lines.
;;
;; Modify the data World to include the words so far, represented
;; as a string. You will also have modify the functions you've written so far.
;; But, if you carefully followed the design recipe, the changes will be minor.
;;
;; NOTE: Your program will show the words entered so far, but it will not
;; check that the player does not enter a duplicate word. We will address that
;; problem later.

;; [TODO] Modifications
(define-struct worldv2 [available-letters words-so-far partial-word])
;; A World is a (make-world Letters String String)
;; Interpretation: A (make-world Letters String String) represents the
;; the avaiable letters a user to can use to make a word,
;; the words gotten so far and the partial word the user has entered:

;; available-letters is the possible Letters the user can use

;; words-so-far is a list of full words the user has entered

;; partial-word is the series of words the user has entered seperated
;; by "/n" as well as letters the user has currently entered

(define worldv2-1 (make-worldv2 letters-set-1 "" "BEAD"))
(define worldv2-2 (make-worldv2 letters-set-1 "" ""))

;; worldv2 : Worldv2 -> ???
(define (worldv2-template w)
  (... (worldv2-available-letters w) ...
       (worldv2-words-so-far w) ...
       (worldv2-partial-word w) ...))

;; world->image : World -> Image
;; Displays the information in World as an Image
(define (world->imagev2 w)
  (place-image
   (text (string-append (worldv2-words-so-far w) (worldv2-partial-word w)) 12 "black")
   180 100
   (draw-letters (worldv2-available-letters w))))

;; key-pressed : World KeyEvent -> World
;; updates the words-so-far and partial-word information in the World
(define (key-pressedv2 w key)
  (cond
    [(string=? key (letters-main (worldv2-available-letters w)))
     (make-worldv2 (worldv2-available-letters w)  (worldv2-words-so-far w)
                   (string-append (worldv2-partial-word w)
                                  (letters-main (worldv2-available-letters w))))]
    [(string=? key (letters-letter1 (worldv2-available-letters w)))
     (make-worldv2 (worldv2-available-letters w) (worldv2-words-so-far w)
                   (string-append (worldv2-partial-word w)
                                  (letters-letter1 (worldv2-available-letters w))))]
    [(string=? key (letters-letter2 (worldv2-available-letters w)))
     (make-worldv2 (worldv2-available-letters w) (worldv2-words-so-far w)
                   (string-append (worldv2-partial-word w)
                                  (letters-letter2 (worldv2-available-letters w))))]
    [(string=? key (letters-letter3 (worldv2-available-letters w)))
     (make-worldv2 (worldv2-available-letters w) (worldv2-words-so-far w)
                   (string-append (worldv2-partial-word w)
                                  (letters-letter3 (worldv2-available-letters w))))]
    [(string=? key (letters-letter4 (worldv2-available-letters w)))
     (make-worldv2 (worldv2-available-letters w) (worldv2-words-so-far w)
                   (string-append (worldv2-partial-word w)
                                  (letters-letter4 (worldv2-available-letters w))))]
    [(and (string=? key "\r")
          (string-contains? (letters-main (worldv2-available-letters w)) (worldv2-partial-word w)))
     (make-worldv2 (worldv2-available-letters w)
                   (string-append (worldv2-words-so-far w) (worldv2-partial-word w) "\n") "")]
    [else w]))

(define (playv2 w)
  (big-bang w
    (to-draw world->imagev2)
    (on-key key-pressedv2)))

(playv2 worldv2-2)