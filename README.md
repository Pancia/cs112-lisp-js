# loki-lang
Public repo for ucsc's winter 2015 CMPS 112 class project involving the creation of a lisp that compiles down to javascript, python, and maybe more.

## Note for Graders
* Check out the wiki for instructions on how to install and run
* The folders ttt-loki-[js|py] contain the tictactoe implementations in both pure js|py and loki
* The file marked as ttt-base.[js|py] is the game written in js|py
* The file marked as ttt-[js|py].loki is the game in loki, and has been compiled down to ttt-*.loki.[html|html.js|py]

## Scrum Board
[Trello Scrum Board](https://trello.com/b/xjlNNkQT/cs112-loki-scrum-board)

## Links & Resources
[Presentation Slides](http://goo.gl/AE8GXn)

### Python Game Engines
* [Pygame] (http://www.pygame.org/news.html)
* [Kivy] (http://kivy.org/#home)
* [Python GUIs] (https://wiki.python.org/moin/GuiProgramming)

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
* TLDR: `>? cabal sandbox init` -> `>? cabal install` -> `>? cabal run -- $args`
* See src/Main.hs options for flags and their description.
