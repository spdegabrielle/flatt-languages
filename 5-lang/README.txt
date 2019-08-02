The "world.rkt" module now uses a non-S-expression syntax, as enabled
through the change of the first line to

  #lang reader "txtadv-reader.rkt"

The new "txtadv-reader.rkt" module parses the syntax of "world.rkt"
and generates the original form as a syntax object.

The "txtadv.rkt" module language is unchanged, since
"txtadv-reader.rkt" converts "world.rkt" to its old form.
