;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8-full) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require test-engine/racket-tests)
(require racket/list)


;; A Letters is one of
;; - [NE-List-of 1String]
;; Interpretation:
;; A [NE-List-of 1String] represents the letters that are available for a game of Spelling Bee.
;; where the last letter in the list is the required letter

;; letters-template : Letters -> ?
(define (letters-template lo1s)
  (...
   (cond
     [(empty? (rest lo1s)) ...
      (first lo1s) ...]
     [(cons? (rest lo1s)) ...
      (first lo1s) ...
      (letters-template (rest lo1s)) ...])))

(define LETTERS-1 (list "c" "o" "e" "b" "w" "a" "l"))
(define LETTERS-2 (list "z" "x" "w" "q" "y" "j" "k"))
(define LETTERS-3 (list "h" "e" "a" "t" "h" "l" "y"))
(define DICTIONARY (read-lines "words.txt"))

;; word->image : String -> Image
;; Displays a word as an image with the color and size that we want
(define (word->image w)
  (text w 20 "black"))

(check-expect (word->image "hello") (text "hello" 20 "black"))
(check-expect (word->image "a") (text "a" 20 "black"))

;; letter->image : 1String Color -> Image
;; Displays a single letter as an image
(define (letter->image s c)
  (overlay (text s 20 c)
           (square 100 "outline" "black")))
 
(check-expect (letter->image "X" "blue")
              (overlay (text "X" 20 "blue") (square 100 "outline" "black")))
(check-expect (letter->image "hello" "red")
              (overlay (text "hello" 20 "red") (square 100 "outline" "black")))

;; letters->image : Letters -> Image
;; Displays a Letters.
(define (letters->image lo1s)
  (cond
    [(empty? (rest lo1s))
     (letter->image (first lo1s) "red")]
    [(cons? (rest lo1s))
     (above (letter->image (first lo1s) "blue")
            (letters->image (rest lo1s)))]))

(check-expect (letters->image LETTERS-1)
              (above (letter->image  "c" "blue")
                     (letter->image  "o" "blue")
                     (letter->image  "e" "blue")
                     (letter->image  "b" "blue")
                     (letter->image  "w" "blue")
                     (letter->image  "a" "blue")
                     (letter->image  "l" "red")))

(check-expect (letters->image LETTERS-2)
              (above (letter->image  "z" "blue")
                     (letter->image  "x" "blue")
                     (letter->image  "w" "blue")
                     (letter->image  "q" "blue")
                     (letter->image  "y" "blue")
                     (letter->image  "j" "blue")
                     (letter->image  "k" "red")))
                       
(define-struct world [letters word-so-far points constructed-words])
;; A World is a (make-world Letters String Nat [List-of String])
;; Interpretation: The letters on the board, the word the user has typed so far, the number of points
;; and the words the user has already entered in a string with newlines.

;; world-template : World -> ?
(define (world-template w)
  (... (letters-template (world-letters w)) ...
       (world-word-so-far w) ...
       (los-template (world-constructed-words w)) ...
       (world-points w) ...))

(define INIT-WORLD-1 (make-world LETTERS-1 "" 0 (list "Words so far:")))
(define INIT-WORLD-2 (make-world LETTERS-1 "bell" 0 (list "Words so far:")))
(define INIT-WORLD-3 (make-world LETTERS-1 "" 1 (list "Words so far:" "bell")))
(define INIT-WORLD-4 (make-world LETTERS-3 "healthy" 0 (list "Words so far:")))

;; world->image : World -> Image
;; Display a World.
(define (world->image w)
  (above
   (text "How many words can you construct" 14 "red")
   (beside/align
    "top"
    (above (letters->image (world-letters w)) (word->image (world-word-so-far w)))
    (word->image (foldr (λ (a b) (string-append b "\n" a))
                        (first (world-constructed-words w)) (rest (world-constructed-words w)))))
   (text (string-append "Score: " (number->string (world-points w))) 14 "red")))


(check-expect (world->image INIT-WORLD-1)
              (above
               (text "How many words can you construct" 14 "red")
               (beside/align
                "top"
                (above (letters->image LETTERS-1)
                       (word->image ""))
                (word->image "Words so far:"))
               (text  "Score: 0" 14 "red")))

(check-expect (world->image INIT-WORLD-2)
              (above
               (text "How many words can you construct" 14 "red")
               (beside/align
                "top"
                (above (letters->image LETTERS-1)
                       (word->image "bell"))
                (word->image "Words so far:"))
               (text  "Score: 0" 14 "red")))

(check-expect (world->image INIT-WORLD-3)
              (above
               (text "How many words can you construct" 14 "red")
               (beside/align
                "top"
                (above (letters->image LETTERS-1)
                       (word->image ""))
                (word->image "Words so far:\nbell"))
               (text  "Score: 1" 14 "red")))

;; available-letter? : Letters String -> Boolean
;; Check if a letter is available.
(define (available-letter? lo1s k)
  (ormap (λ (s) (string=? k s)) lo1s))

(check-expect (available-letter? LETTERS-1 "b") #true)
(check-expect (available-letter? LETTERS-1 "q") #false)
(check-expect (available-letter? LETTERS-2 "e") #false)

;; key-pressed : World KeyEvent -> World
;; Produce a new World in reaction to a key-press.
(define (key-pressed w k)
  (cond
    [(available-letter? (world-letters w) k)
     (letter-available w k)]
    [(and (string=? k "\b") (> (string-length (world-word-so-far w)) 0))
     (backspace w)]
    [(and (string=? k "\r")
          ;; Checks if require letter is in the guessed word
          (string-contains? (last (world-letters w)) (world-word-so-far w))
          ;; Checks if the length of the word is at least 4
          (> (string-length (world-word-so-far w)) 3)
          ;; Checks if the word has not already been guessed
          (not (ormap (λ (s) (string=? (world-word-so-far w) s)) (world-constructed-words w)))
          ;; Checks if the word is in the dictionary
          (ormap (λ (s) (string=? (world-word-so-far w) s)) DICTIONARY))
     (valid-guess w)]
    [else w]))

(check-expect (key-pressed INIT-WORLD-2 "\r") INIT-WORLD-3)
(check-expect (key-pressed INIT-WORLD-2 "\b") (make-world LETTERS-1 "bel" 0 (list "Words so far:")))
(check-expect (key-pressed INIT-WORLD-3 "x") INIT-WORLD-3)
(check-expect (key-pressed INIT-WORLD-1 "b") (make-world LETTERS-1 "b" 0 (list "Words so far:")))
(check-expect (key-pressed INIT-WORLD-4 "\r")
              (make-world LETTERS-3 "" 11 (list "Words so far:" "healthy")))


;; letter-available : World KeyEvent -> World
;; Adds the letter pressed to the typed word
(define (letter-available a k)
  (make-world (world-letters a) (string-append (world-word-so-far a) k)
              (world-points a) (world-constructed-words a)))

(check-expect (letter-available INIT-WORLD-1 "a") (make-world LETTERS-1 "a" 0 (list "Words so far:")))
(check-expect (letter-available INIT-WORLD-2 "s") (make-world LETTERS-1 "bells" 0
                                                              (list "Words so far:")))

;; backspace : World -> World
;; Removes the most recent letter from the typed word
(define (backspace a)
  (make-world (world-letters a)
              (substring (world-word-so-far a) 0 (- (string-length (world-word-so-far a)) 1))
              (world-points a)
              (world-constructed-words a)))

(check-expect (backspace INIT-WORLD-2) (make-world LETTERS-1 "bel" 0
                                                   (list "Words so far:")))
(check-expect (backspace INIT-WORLD-4) (make-world LETTERS-3 "health" 0
                                                   (list "Words so far:")))

;; valid-guess : World -> World
;; Calculate the points each valid word is worth, and updates the world by adding the score to it
;; and adds the word to constructed-words and it empties word-so-far
(define (valid-guess a)
  (make-world (world-letters a)
              ""
              (local
                [; bonus-score : String [NE-List-of 1String] -> Number
                 ; checks if the guess word contains all seven letters
                 (define (bonus-score s lo1s)
                   (if (andmap (λ (c) (string-contains? c s)) lo1s) 7 0))]
                (+ (world-points a) (bonus-score (world-word-so-far a) (world-letters a))
                   (- (string-length (world-word-so-far a)) 3)))
              (append (world-constructed-words a) (list (world-word-so-far a)))))

(check-expect (valid-guess INIT-WORLD-2) INIT-WORLD-3)
(check-expect (valid-guess INIT-WORLD-4) (make-world LETTERS-3 "" 11
                                                     (list "Words so far:" "healthy")))




;; play : World -> World
;; Uses big-bang to play a game of Spelling Bee, given Letters.
(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))

(check-expect (play INIT-WORLD-2) (big-bang INIT-WORLD-2 (to-draw world->image) (on-key key-pressed)))
(check-expect (play INIT-WORLD-3) (big-bang INIT-WORLD-3 (to-draw world->image) (on-key key-pressed)))


