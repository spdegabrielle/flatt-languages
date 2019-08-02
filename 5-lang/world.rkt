#lang reader "txtadv-reader.rkt"

===VERBS===

north, n
 "go north"

south, s
 "go south"

east, e
 "go east"

west, w
 "go west"

up
 "go up"

down
 "go down"

in, enter
 "enter"

out, leave
 "leave"

get _, grab _, take _
 "take"

put _, drop _, leave _
 "drop"

open _, unlock _
 "open"

close _, lock _
 "close"

knock _

quit, exit
 "quit"

look, show
 "look"

inventory
 "check inventory"

help

save

load


===EVERYWHERE===

quit 
 (begin
  (printf "Bye!\n")
  (exit))

look 
 (show-current-place)

inventory 
 (show-inventory)

save
 (save-game)

load
 (load-game)

help
 (show-help)


===THINGS===

---cactus---
get
  "Ouch!"

---door---
open 
  (if (have-thing? key)
      (begin
        (set-thing-state! door 'open)
        "The door is now unlocked and open.")
      "The door is locked.")

close
  (begin
   (set-thing-state! door #f)
   "The door is now closed.")

knock
  "No one is home."

---key---

get
  (if (have-thing? key)
      "You already have the key."
      (begin
        (take-thing! key)
        "You now have the key."))

put
  (if (have-thing? key)
      (begin
        (drop-thing! key)
        "You have dropped the key.")
      "You don't have the key.")

---trophy---

get
  (begin
   (take-thing! trophy)
   "You win!")


===PLACES===

---meadow---
"You're standing in a meadow. There is a house to the north."
[]

north
 house-front

south
 desert


---house-front---
"You are standing in front of a house."
[door]

in 
  (if (eq? (thing-state door) 'open)
      room
      "The door is not open.")

south
  meadow


---desert---
"You're in a desert. There is nothing for miles around."
[cactus, key]

north
  meadow

south
  desert

east
  desert

west
  desert


---room---
"You're in the house."
[trophy]

out
  house-front
