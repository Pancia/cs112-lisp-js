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
                ,("fn", (["plus(1, [2,3])"], "(+ 1 '(2 3))"))]
        lisp2js = L.catch . liftM (map L.lisp2js) . L.readExpr

testJsLisp :: [IO T.Test]
testJsLisp = fmap (\(name, (l, r)) -> do r' <- lisp2jsOutput r; return $ testCase name (l @=? r')) tests
    where
        tests = [("addition", ("9\n", "(log. (+ 2 3 4))"))]
        lisp2jsOutput lisp = do
            let js = L.catch . liftM (map L.lisp2js) . L.readExpr $ lisp
            helperFns <- readFile "helperFunctions.js"
            writeFile "out.js" $ helperFns ++ intercalate ";\n" js ++ ";"
            (_, Just hout, _, _) <- createProcess $ (proc "jsc" ["out.js"]) { std_out = CreatePipe }
            hGetContents hout
