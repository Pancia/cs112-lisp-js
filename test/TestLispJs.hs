import Data.Monoid
import Control.Monad
import Control.Applicative hiding (Const)

import Text.Parsec (runParser)
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

import Data.Map (fromList)

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
        tests = [("list of numbers", ([List (fromList [("fileType","js")]) [Number (fromList [("fileType","js")]) 3, Number (fromList [("fileType","js")]) 4]], "(3 4)"))
                ,("list of atoms", ([List (fromList [("fileType","js")]) [Atom (fromList [("fileType","js")]) "foo", Atom (fromList [("fileType","js")]) "bar"]], "(foo bar)"))
                ,("def obj", ([New (fromList [("fileType","js")]) "Number" [Number (fromList [("fileType","js")]) 5]] , "(new Number 5)"))
                ,("defclass", ([DefClass (fromList [("fileType","js")]) "Foo" (Const (fromList [("fileType","js")]) [] $ Number (fromList [("fileType","js")]) 5) [] []],
                              "(defclass Foo ([] 5))"))
                ,("classvar", ([DefClass (fromList [("fileType","js")]) "Foo" (Const (fromList [("fileType","js")]) [] $ Number (fromList [("fileType","js")]) 5) [] [Classvar (fromList [("fileType","js")]) "bar" $ Number (fromList [("fileType","js")]) 3]],
                              "(defclass Foo ([] 5) (bar 3))"))
                ,("classfn", ([DefClass (fromList [("fileType","js")]) "Foo" (Const (fromList [("fileType","js")]) [] $ Number (fromList [("fileType","js")]) 5) [Classfn (fromList [("fileType","js")]) "bar" [] $ Number (fromList [("fileType","js")]) 3] []],
                              "(defclass Foo ([] 5) (bar [] 3))"))
                ,("classfn n classvar",([DefClass (fromList [("fileType","js")]) "Foo" (Const (fromList [("fileType","js")]) [] $ Number (fromList [("fileType","js")]) 5) [Classfn (fromList [("fileType","js")]) "bar" [] $ Number (fromList [("fileType","js")]) 3] [Classvar (fromList [("fileType","js")]) "tar" $ Number (fromList [("fileType","js")]) 2]],
                              "(defclass Foo ([] 5) (bar [] 3) (tar 2) )")) ]
        readExpr' = U.catch . readExpr

testToJS :: [T.Test]
testToJS = fmap (\(name, (l, r)) -> testCase name (l @=? lisp2js r)) tests
    where
        tests = [("log", (["loki.print(5)"], "(print 5)"))
                ,("plus", (["loki.plus(1, [2, 3])"], "(+ 1 '(2 3))"))
                ,("def", (["var foo = 5"],"(def foo 5)"))
                ,("def1", (["var foo = loki.plus(3, 2)"],"(def foo (+ 3 2))"))
                ,("fn", (["function () {\nreturn true\n}"], "(fn [] true)"))
                ,("fn+", (["function (x) {\nreturn loki.plus(x, 2)\n}"], "(fn [x] (+ x 2))"))
                ,("def&fn", (["var foo = function (x, y) {\nreturn loki.minus(x, y)\n}"], "(def foo (fn [x y] (- x y)))"))
                ,("defn&call", (["var f = function () {\nreturn true\n}", "loki.print(f())"],"(def f (fn [] true)) (print (f))"))]
        lisp2js = fmap JS.toJS . U.catch . liftM (JS.translate <$>) . readExpr

testExecJS :: [IO T.Test]
testExecJS = fmap (\(name, (l, r)) -> do r' <- lisp2execJS r name; return $ testCase name (l @=? r')) tests
    where
        tests = [("addition", ("9", "(print (+ 2 3 4))"))
                ,("defn&call", ("true", "(def f (fn [] true)) (print (f))"))
                ,("defn+&call", ("5", "(def f (fn [] (+ 2 3))) (print (f))"))
                ,("defn+args&call", ("10", "(def f (fn [x] (+ x 1))) (print (f 9))"))]
        lisp2execJS :: String -> String -> IO String
        lisp2execJS lisp filename = do
            let outDir = FS.decodeString "test-out"
                outFile = outDir FS.</> FS.decodeString filename
            SH.mktree outDir
            js <- JS.formatJs . fmap JS.toJS . U.catch . liftM (JS.translate <$>) . readExpr $ lisp
            SH.output outFile $ return $ T.pack js
            let jsOutput = SH.inproc (T.pack $ U.caseWindowsOrOther "cscript" "jsc") [FS.encode outFile] SH.empty
            SH.fold jsOutput (T.unpack <$> F.mconcat)

readExpr :: String -> Either U.CompilerError [U.LokiVal]
readExpr input = case runParser P.parseExpr "js" "lisp-js" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
