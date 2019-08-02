The starting line of "world.rkt" has changed, and the ending call to
`start-game' has been replaced by just the starting place name. The
content of "world.rkt" is also constrained to have a `define-verbs'
form followed by a `define-everywhere' form, but the previous version
fit that constraint anyway.

The "txtadv.rkt" module changed only in defining `module-begin' and
exporting it as a replacement `#%module-begin'.
