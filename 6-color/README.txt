To make this version work, you need the directory containing this file
to be installed as the "txtadv" collection. One way to do that is with
the shell command

   raco link --name txtadv .

in the directory containing this file.


The "world.rkt" module is the same as before, except that its first
line is now

  #lang txtadv

The "txtadv.rkt" module is unchanged.

Compared to the previous implementation, "txtadv-reader.rkt" is now
"lang/reader.rkt", which is required to match Racket's protocol for
resolving `#lang txtadv' to its reader module. The only other change
is that a `get-info' function points to the "color.rkt" module in
"lang" to implement syntax coloring.
