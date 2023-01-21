Creating Languages in Racket
============================

This repository contains the code for [Creating Languages in Racket](https://cacm.acm.org/magazines/2012/1/144809-creating-languages-in-racket/fulltext) by Matthew Flatt

Note, there is another copy of this article's code on GitHub, along with a Chinese translation of the paper: https://github.com/ice051/CLR

To see the evolution of this article's code with your favourite _difftool_, you can use the following commands:
```
difftool 0-longhand/ 1-monolith/
difftool 1-monolith/txtadv+world.rkt 2-modules/txtadv.rkt
difftool 1-monolith/txtadv+world.rkt 2-modules/world.rkt
difftool 2-modules/ 3-module-lang/
difftool 3-module-lang/ 4-type/
difftool 4-type/ 5-lang/
difftool 5-lang/ 6-color/
difftool 5-lang/txtadv-reader.rkt 6-color/lang/reader.rkt
```

To ease directories comparison you can remove `compiled` directories with a Bash command:
```
find . -type d -name compiled | xargs rm -r
```
... be careful with `rm` command, though.
