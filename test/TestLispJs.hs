import Data.Monoid
import Control.Monad
import Control.Applicative hiding (Const)

import Text.Parsec (parse)
import Control.Monad.Except (throwError)

import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text as T
import qualified Turtle as SH
import qualified Control.Foldl as F

import qualified Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2()
import Test.HUnit
import Test.QuickCheck()

import qualified LispJs as JS
import Utils as U
import qualified Parser as P
import qualified TestLispPy as PY

--TODO: Refactor to different files for testing each src/ file
main :: IO ()
main = do tests' <- sequence testExecJS
          T.defaultMainWithOpts (tests ++ tests') mempty
    where
        tests = testToJS ++ testReadExpr ++ PY.tests

testReadExpr :: [T.Test]
testReadExpr = fmap (\(name, (l, r)) -> testCase name (l @=? readExpr' r)) tests
    where
        tests = [("list of numbers", ([List [Number 3, Number 4]], "(3 4)"))
                ,("list of atoms", ([List [Atom "foo", Atom "bar"]], "(foo bar)"))
                ,("def obj", ([New "Number" [Number 5]] , "(new Number 5)"))
                ,("defclass", ([DefClass "Foo" (Const [] $ Number 5) [] []],
                              "(defclass Foo ([] 5))"))
                ,("classvar", ([DefClass "Foo" (Const [] $ Number 5) [] [Classvar "bar" $ Number 3]],
                              "(defclass Foo ([] 5) (bar 3))"))
                ,("classfn", ([DefClass "Foo" (Const [] $ Number 5) [Classfn "bar" [] $ Number 3] []],
                              "(defclass Foo ([] 5) (bar [] 3))"))
                ,("classfn n classvar",([DefClass "Foo" (Const [] $ Number 5) [Classfn "bar" [] $ Number 3] [Classvar "tar" $ Number 2]],
                              "(defclass Foo ([] 5) (bar [] 3) (tar 2) )")) ]
        readExpr' = U.catch . readExpr

testToJS :: [T.Test]
testToJS = fmap (\(name, (l, r)) -> testCase name (l @=? lisp2js r)) tests
    where
        tests = [("log", (["print(5)"], "(log 5)"))
                ,("plus", (["plus(1, [2, 3])"], "(+ 1 '(2 3))"))
                ,("def", (["var foo = 5"],"(def foo 5)"))
                ,("def1", (["var foo = plus(3, 2)"],"(def foo (+ 3 2))"))
                ,("fn", (["function () {\nreturn true\n}"], "(fn [] true)"))
                ,("fn+", (["function (x) {\nreturn plus(x, 2)\n}"], "(fn [x] (+ x 2))"))
                ,("def&fn", (["var foo = function (x, y) {\nreturn minus(x, y)\n}"], "(def foo (fn [x y] (- x y)))"))
                ,("defn&call", (["var f = function () {\nreturn true\n}", "print(f())"],"(def f (fn [] true)) (log (f))"))]
        lisp2js = fmap JS.toJS . U.catch . liftM (JS.translate <$>) . readExpr

testExecJS :: [IO T.Test]
testExecJS = fmap (\(name, (l, r)) -> do r' <- lisp2execJS r name; return $ testCase name (l @=? r')) tests
    where
        tests = [("addition", ("9", "(log (+ 2 3 4))"))
                ,("defn&call", ("true", "(def f (fn [] true)) (log (f))"))
                ,("defn+&call", ("5", "(def f (fn [] (+ 2 3))) (log (f))"))
                ,("defn+args&call", ("10", "(def f (fn [x] (+ x 1))) (log (f 9))"))]
        lisp2execJS :: String -> String -> IO String
        lisp2execJS lisp filename = do
            let outDir = FS.decodeString "test-out"
                outFile = outDir FS.</> FS.decodeString filename
            SH.mktree outDir
            js <- JS.formatJs . fmap JS.toJS . U.catch . liftM (JS.translate <$>) . readExpr $ lisp
            SH.output outFile $ return $ T.pack js
            let jsOutput = SH.inproc (T.pack U.getJsExecProgName) [FS.encode outFile] SH.empty
            SH.fold jsOutput (T.unpack <$> F.mconcat)

readExpr :: String -> Either U.CompilerError [LispVal]
readExpr input = case parse P.parseExpr "lisp-js" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
