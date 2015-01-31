import Data.Monoid
import Control.Monad

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2()
import Test.HUnit
import Test.QuickCheck()

import qualified LispJs as L

main = defaultMainWithOpts tests mempty
        where
            tests = testlisp2js

testlisp2js :: [Test.Framework.Test]
testlisp2js = fmap (\(name, (l, r)) -> testCase name (l @?= lisp2js r)) tests
        where
            tests = [("testing log", ("print(5)", "(log. 5)"))
                    ,("testing fn", ("plus(1, [2,3])", "(plus 1 '(2 3))"))]
            lisp2js = L.catch . liftM L.lisp2js . L.readExpr
