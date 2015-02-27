module TestLispJs where

import Control.Applicative hiding (Const)
import Control.Monad
import Test.Framework.Providers.QuickCheck2()
import TestUtils

import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified LispJs as JS
import qualified Test.Framework as T
import qualified Turtle as SH
import qualified Utils as U

tests :: IO [T.Test]
tests = sequence . join $ runTest "js" <$> [testJsParser, testToJS, testExecJS]

testJsParser :: LokiTests
testJsParser = (,) <$> ["sexp"]
                   <*> [parseJS]
    where parseJS (_, lisp) = do
            let parsed = U.catch $ show <$> readExpr "js" lisp
            return ("parse", parsed)

testToJS :: LokiTests
testToJS = (,) <$> ["helpers", "specials"]
               <*> [toJS]
    where toJS (_, lisp) = do
            let translated = U.catch . liftM (JS.translate <$>) $ readExpr "js" lisp
                jsStr = concat . fmap JS.toJS $ translated
            return ("convert", jsStr)

testExecJS :: LokiTests
testExecJS = (,) <$> ["simple", "complex"]
                 <*> [execJS]
    where execJS (file, lisp) = do
            js <- JS.formatJs . fmap JS.toJS . U.catch
                  . liftM (JS.translate <$>) . readExpr "js" $ lisp
            let outFile = "tests/" ++ file ++ ".out.js"
            writeFile outFile js
            let jsOutput = SH.inproc (T.pack $ U.caseWindowsOrOther "cscript" "jsc")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            (,) <$> return "exec"
                <*> SH.fold jsOutput (T.unpack <$> F.mconcat)
