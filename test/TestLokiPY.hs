module TestLokiPY where

import Control.Applicative hiding (Const)
import Control.Monad
import TestUtils

import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified LokiPY as PY
import qualified Test.Framework as T
import qualified Turtle as SH
import qualified Utils as U

tests :: IO [T.Test]
tests = sequence . join $ runTest "py" <$> [testPyParser, testToPY, testExecPY]

testPyParser :: LokiTests
testPyParser = (,) <$> ["class-object", "def+fn", "specials"
                       ,"primitives", "literals", "helpers"]
                   <*> [parsePY]
    where parsePY (_, lisp) = do
            let parsed = U.catch $ show <$> readExpr "py" lisp
            return ("parse", parsed)

testToPY :: LokiTests
testToPY = (,) <$> ["helpers", "specials", "def+fn"]
               <*> [toPY]
    where toPY (_, lisp) = do
            let translated = U.catch . liftM (PY.translate <$>) $ readExpr "py" lisp
                pyStr = concat . fmap (PY.toPY 0) $ translated
            return ("convert", pyStr)

testExecPY :: LokiTests
testExecPY = (,) <$> ["simple", "complex", "helpers"]
                 <*> [execPY]
    where execPY (file, lisp) = do
            py <- PY.formatPy . fmap (PY.toPY 0) . U.catch
                  . liftM (PY.translate <$>) . readExpr "py" $ lisp
            let outFile = "tests/" ++ file ++ ".out.py"
            writeFile outFile py
            let pyOutput = SH.inproc (T.pack "python")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            (,) <$> return "exec"
                <*> SH.fold pyOutput (T.unpack <$> F.mconcat)
