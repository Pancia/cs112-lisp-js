import Data.Monoid
import Control.Monad
import Control.Applicative

import Text.Parsec (parse)
import Control.Monad.Except (throwError)

import System.Process
import System.IO

import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2()
import Test.HUnit
import Test.QuickCheck()

import qualified LispPy as L
import qualified Utils as U
import qualified Parser as P

--TODO: Refactor to different files for testing each src/ file
main :: IO ()
main = do tests' <- sequence testExecJS
          defaultMainWithOpts (tests ++ tests') mempty
    where
        tests = testToJS ++ testReadExpr

testReadExpr :: [T.Test]
testReadExpr = fmap (\(name, (l, r)) -> testCase name (l @=? readExpr' r)) tests
    where
        tests = [("list of numbers", ([L.List [L.Number 3, L.Number 4]], "(3 4)"))
                ,("list of atoms", ([L.List [L.Atom "foo", L.Atom "bar"]], "(foo bar)"))]
        readExpr' = U.catch . readExpr

testToPY :: [T.Test]
testToPY = fmap (\(name, (l, r)) -> testCase name (l @=? lisp2py r)) tests
    where
        tests = [("log", (["print(5)"], "(log 5)"))
                ,("plus", (["plus(1, [2, 3])"], "(+ 1 '(2 3))"))
                ,("def", (["var foo = 5"],"(def foo 5)"))
                ,("def1", (["var foo = plus(3, 2)"],"(def foo (+ 3 2))"))
                ,("fn", (["function () {\nreturn true\n}"], "(fn [] #t)"))
                ,("fn+", (["function (x) {\nreturn plus(x, 2)\n}"], "(fn [x] (+ x 2))"))
                ,("def&fn", (["var foo = function (x, y) {\nreturn minus(x, y)\n}"], "(def foo (fn [x y] (- x y)))"))
                ,("defn&call", (["var f = function () {\nreturn true\n}", "print(f())"],"(def f (fn [] #t)) (log (f))"))]
        lisp2py = fmap L.toPY . U.catch . liftM (L.translate <$>) . readExpr

testExecPY :: [IO T.Test]
testExecPY = fmap (\(name, (l, r)) -> do r' <- lisp2execPY r name; return $ testCase name (l @=? r')) tests
    where
        tests = [("addition", ("9\n", "(log (+ 2 3 4))"))
                ,("defn&call", ("true\n", "(def f (fn [] #t)) (log (f))"))
                ,("defn+&call", ("5\n", "(def f (fn [] (+ 2 3))) (log (f))"))
                ,("defn+args&call", ("10\n", "(def f (fn [x] (+ x 1))) (log (f 9))"))]
        lisp2execPY lisp fn = do
            _ <- createProcess $ proc "mkdir" ["test-out"]
            py <- L.formatJs . fmap L.toPY . U.catch . liftM (L.translate <$>) . readExpr $ lisp
            let filename = "test-out/" ++ fn
            writeFile filename py
            execPY filename

execPY :: String -> IO String
execPY file = do (_, Just hout, _, _) <- createProcess $ (proc "pyc" [file]) { std_out = CreatePipe }
                 hGetContents hout

readExpr :: String -> Either U.CompilerError [L.LispVal]
readExpr input = case parse P.parseExpr "lisp-py" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
