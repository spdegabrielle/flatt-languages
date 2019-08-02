The old "txtadv+world.rkt" module is now split into two parts:
"txtadv.rkt" and "world.rkt". Aside from the split and necessary
`provide' and `require' declarations, the only change to engine and
world descriptions is the `start-game' call at the end of "world.rkt",
which passes to the game engine all of the declarations that it
formerly used directly.

To play the game, run the "world.rkt" module.
