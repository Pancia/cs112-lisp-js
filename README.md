# cs112-lisp-js
Public repo for ucsc's winter 2015 CMPS 112 class project involving the creation of a (functional) lisp that compiles down to (immutable) js.

## Links & Resources

### Compiler
* [Write yourself a scheme](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps)
* [Write yourself a brainfuck](https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md)

### Parser
* [Haskell parsec basics](http://unbui.lt/#!/post/haskell-parsec-basics)
* [Parsec hackage](http://hackage.haskell.org/package/parsec-3.1.8)

### Cabal: Build tool
* [Haskell.org cabal guide](https://www.haskell.org/cabal/users-guide/installing-packages.html)
* [Intro to cabal sandboxes](https://www.fpcomplete.com/school/to-infinity-and-beyond/older-but-still-interesting/an-introduction-to-cabal-sandboxes-copy)
* [Cabal sandbox workflow](http://chromaticleaves.com/posts/cabal-sandbox-workflow.html)
* TLDR: `>? cabal sandbox init` -> `>? cabal install` -> `>? cabal run $args`

#### Exec JS on cmdl
http://www.phpied.com/javascript-shell-scripting/

### Functional JS
* [Overview of how js is functional](http://www.hunlock.com/blogs/Functional_Javascript)
* [Curry fn in js](http://www.crockford.com/javascript/www_svendtofte_com/code/curried_javascript/index.html)
* [Top choice: mori, ~cljs~ in js](http://swannodette.github.io/mori/)
* [underscore.js](http://underscorejs.org/)
* [functional.js](http://functionaljs.com/)
* [immutable.js](http://facebook.github.io/immutable-js/)

### Proposed Class and Object Design
```Clojure
(defclass name
  ([self params*]
    (self property val)) ;Constructor must be first
  (fn name [angs]
    (body)) ;function declaration
  (varName val)) ;var declaration
  ```
Static instances denoted by ^ So:
```Clojure
^(varName val) ;will be static
```
Objects can be made by:
```Clojure
(def objName (new className params*)) ;makes new object
```

Do we want inheritance?
