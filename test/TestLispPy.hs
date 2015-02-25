module TestLispPy where

import Control.Monad
import Control.Applicative

import Text.Parsec (runParser)
import Control.Monad.Except (throwError)

import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2()
import Test.HUnit
import Test.QuickCheck()

import qualified LispPy as L
import qualified Utils as U
import qualified Parser as P

--TODO: Refactor to different files for testing each src/ file
tests :: [T.Test]
tests = testToPY

testToPY :: [T.Test]
testToPY = fmap (\(name, (l, r)) -> testCase name (l @=? lisp2py r)) tests'
    where
        tests' = [("def", (["foo = 5"],"(def foo 5)"))]
        lisp2py = fmap L.toPY . U.catch . liftM (L.translate <$>) . readExpr

readExpr :: String -> Either U.CompilerError [U.LokiVal]
readExpr input = case runParser P.parseExpr "py" "lisp-py" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
