import Data.Monoid
import Data.List
import Control.Monad

import System.Process
import System.IO

import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2()
import Test.HUnit
import Test.QuickCheck()

import qualified LispJs as L

main :: IO ()
main = do tests' <- sequence testJsLisp
          defaultMainWithOpts (tests ++ tests') mempty
    where
        tests = testLisp2js ++ testReadExpr

testReadExpr :: [T.Test]
testReadExpr = fmap (\(name, (l, r)) -> testCase name (l @?= readExpr r)) tests
    where
        tests = [("list of numbers", ([L.List [L.Number 3, L.Number 4]], "(3 4)"))
                ,("list of atoms", ([L.List [L.Atom "foo", L.Atom "bar"]], "(foo bar)"))]
        readExpr = L.catch . L.readExpr

testLisp2js :: [T.Test]
testLisp2js = fmap (\(name, (l, r)) -> testCase name (l @?= lisp2js r)) tests
    where
        tests = [("log.", (["print(5)"], "(log. 5)"))
                ,("plus", (["plus(1, [2,3])"], "(+ 1 '(2 3))"))
                ,("def", (["var foo = 5"],"(def foo 5)"))
                ,("def1", (["var foo = plus(3, 2)"],"(def foo (+ 3 2))"))
                ,("fn", (["function () {return true}"], "(fn [] #t)"))
                ,("fn+", (["function (x) {return plus(x, 2)}"], "(fn [x] (+ x 2))"))
                ,("def&fn", (["var foo = function (x, y) {return minus(x, y)}"], "(def foo (fn [x y] (- x y)))"))
                ,("defn&call", (["var f = function () {return true}", "print(f())"],"(def f (fn [] #t)) (log. (f))"))]
        lisp2js = L.catch . liftM (map L.lisp2js) . L.readExpr

testJsLisp :: [IO T.Test]
testJsLisp = fmap (\(name, (l, r)) -> do r' <- lisp2jsOutput r name; return $ testCase name (l @=? r')) tests
    where
        tests = [("addition", ("9\n", "(log. (+ 2 3 4))"))
                ,("defn&call", ("true\n", "(def f (fn [] #t)) (log. (f))"))
                ,("defn+&call", ("5\n", "(def f (fn [] (+ 2 3))) (log. (f))"))
                ,("defn+args&call", ("10\n", "(def f (fn [x] (+ x 1))) (log. (f 9))"))]
        lisp2jsOutput lisp fn = do
            let js = L.catch . liftM (map L.lisp2js) . L.readExpr $ lisp
            helperFns <- readFile "helperFunctions.js"
            let filename = "." ++ fn
            writeFile filename $ helperFns ++ intercalate ";\n" js ++ ";"
            (_, Just hout, _, _) <- createProcess $ (proc "jsc" [filename]) { std_out = CreatePipe }
            hGetContents hout
