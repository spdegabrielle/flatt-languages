#lang racket
(require "txtadv.rkt")

;; Verbs ----------------------------------------
;; Declare all the verbs that can be used in the game.
;; Each verb has a canonical name, a `_' if it needs
;; an object (i.e., a transitive verb), a set of aliases, 
;; and a printed form.

(define-verbs all-verbs
  [north (= n) "go north"]
  [south (= s) "go south"]
  [east (= e) "go east"]
  [west (= w) "go west"]
  [up (=) "go up"]
  [down (=) "go down"]
  [in (= enter) "enter"]
  [out (= leave) "leave"]  
  [get _ (= grab take) "take"]
  [put _ (= drop leave) "drop"]
  [open _ (= unlock) "open"]
  [close _ (= lock) "close"]
  [knock _]
  [quit (= exit) "quit"]
  [look (= show) "look"]
  [inventory (=) "check inventory"]
  [help]
  [save]
  [load])

;; Global actions ----------------------------------------
;; Handle verbs that work anywhere.

(define-everywhere everywhere-actions
  ([quit (begin (printf "Bye!\n") (exit))]
   [look (show-current-place)]
   [inventory (show-inventory)]
   [save (save-game)]
   [load (load-game)]
   [help (show-help)]))

;; Objects ----------------------------------------
;; Each object handles a set of transitive verbs.

(define-thing cactus
  [get "Ouch!"])

(define-thing door
  [open (if (have-thing? key)
            (begin
              (set-thing-state! door 'open)
              "The door is now unlocked and open.")
            "The door is locked.")]
  [close (begin
           (set-thing-state! door #f)
           "The door is now closed.")]
  [knock "No one is home."])

(define-thing key
  [get (if (have-thing? key)
           "You already have the key."
           (begin
             (take-thing! key)
             "You now have the key."))]
  [put (if (have-thing? key)
           (begin
             (drop-thing! key)
             "You have dropped the key.")
           "You don't have the key.")])

(define-thing trophy
  [get (begin
         (take-thing! trophy)
         "You win!")])

;; Places ----------------------------------------
;; Each place handles a set of non-transitive verbs.

(define-place meadow
  "You're standing in a meadow. There is a house to the north."
  []
  ([north house-front]
   [south desert]))

(define-place house-front
  "You are standing in front of a house."
  [door]
  ([in (if (eq? (thing-state door) 'open)
           room
           "The door is not open.")]
   [south meadow]))

(define-place desert
  "You're in a desert. There is nothing for miles around."
  [cactus key]
  ([north meadow]
   [south desert]
   [east desert]
   [west desert]))

(define-place room
  "You're in the house."
  [trophy]
  ([out house-front]))


;; Go! ---------------------------------------------------

(start-game meadow
            all-verbs
            everywhere-actions)
