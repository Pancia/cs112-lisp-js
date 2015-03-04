module TestLokiJS where

import Data.Maybe
import Control.Applicative hiding (Const)
import Control.Monad
import Test.Framework.Providers.QuickCheck2()
import TestUtils

import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified LokiJS as JS
import qualified Test.Framework as TF
import qualified Turtle as SH
import qualified Utils as U

tests :: IO [TF.Test]
tests = sequence . join $ runTest "js" <$> [testJsParser, testToJS, testExecJS]

testJsParser :: LokiTests
testJsParser = (,) <$> ["class-object", "def+fn", "specials"
                       ,"primitives", "literals", "helpers"]
                   <*> [parseJS]
    where parseJS (_, lisp) = do
            let parsed = U.catch $ show <$> readExpr "js" lisp
            return ("parse", parsed)

testToJS :: LokiTests
testToJS = (,) <$> ["class-object", "helpers", "specials", "def+fn"]
               <*> [toJS]
    where toJS (_, lisp) = do
            let translated = U.catch . liftM (JS.translate <$>) $ readExpr "js" lisp
                jsStr = concat . mapMaybe JS.toJS $ translated
            return ("convert", jsStr)

testExecJS :: LokiTests
testExecJS = (,) <$> ["class-object", "simple", "complex", "helpers"]
                 <*> [execJS]
    where execJS (file, lisp) = do
            js <- JS.formatJs . fmap JS.toJS . U.catch
                  . liftM (JS.translate <$>) . readExpr "js" $ lisp
            let outFile = "tests/" ++ file ++ ".out.js"
            writeFile outFile js
            let jsOutput = SH.inproc (T.pack "node")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            (,) <$> return "exec"
                <*> SH.fold jsOutput (T.unpack <$> F.mconcat)
