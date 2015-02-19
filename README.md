# cs112-lisp-js
Public repo for ucsc's winter 2015 CMPS 112 class project involving the creation of a (functional) lisp that compiles down to (immutable) js.

## Scrum Board
To Do:
* Create `LispVal` constructors for class/object stuff
* Array indexing (helper fn similar to get in clj)
* Parse new syntax
  * Parse defclass, constructor, and name
  * Parse static declarations
  * Parse instance declarations (variables and methods)
  * Parse / syntax for static/class
* Convert each new `LispVal` into JavaScript
* Convert each new `LispVal` into Python

In Progress:
* JS in browser/compiling to HTML (Rachelle)
* Syntax changes (Anthony)
* Conditional tag syntax ```(#+langName expr) ``` (Neeraj)
* Make game in our language (Francisco)

Finished:
* Parse new syntax
 * Parse . syntax for objects
 * Parse new constructor (`(new classname args*)`)

Stretch Goals:
* Compile down to a dynamically-typed language
* Compile down to a statically-typed language
* Allow namespaces/modules/header files
* Allow macros


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

### Proposed Class and Object Design
```Clojure
(defclass SuperClass/Name
  ([params*]
    {var val
    ,var2 val2}) ;Constructor must be first
  (fnName [args*]
    (body)) ;function declaration
  ^(fnName [args*] 
    (body)) ;static function declaration
  (varName val) ;var declaration
  ^(varName val)) ;static var declaration
  
  (def objName (new className params*)) ;makes new object, objName, of type className
  (.method-or-prop objName args*)
  
  Class/prop
  (Class/method args*)
```
