import Data.Monoid
import Control.Monad

import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2()
import Test.HUnit
import Test.QuickCheck()

import qualified LispJs as L

main :: IO ()
main = defaultMainWithOpts tests mempty
        where
            tests = testLisp2js ++ testReadExpr

testReadExpr :: [T.Test]
testReadExpr = fmap (\(name, (l, r)) -> testCase name (l @?= readExpr r)) tests
    where
        tests :: [(String, (L.LispVal, String))]
        tests = [("list of numbers", (L.List [L.Number 3, L.Number 4], "(3 4)"))
                ,("list of atoms", (L.List [L.Atom "foo", L.Atom "bar"], "(foo bar)"))]
        readExpr :: String -> L.LispVal
        readExpr = L.catch . L.readExpr

testLisp2js :: [T.Test]
testLisp2js = fmap (\(name, (l, r)) -> testCase name (l @?= lisp2js r)) tests
    where
        tests = [("log.", ("print(5)", "(log. 5)"))
                ,("fn", ("plus(1, [2,3])", "(plus 1 '(2 3))"))]
        lisp2js = L.catch . liftM L.lisp2js . L.readExpr
