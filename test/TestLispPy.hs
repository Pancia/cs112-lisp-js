module TestLispPy where

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

import qualified LispPy as PY
import qualified Utils as U
import qualified Parser as P

type LokiTest = [(String, (String, String) -> IO String)]

tests :: IO [T.Test]
tests = sequence . join $ runTest <$> [testPyParser, testToPY, testExecPY]

testPyParser :: LokiTest
testPyParser = (,) <$> ["sexp"] <*> [return . U.catch . (show <$>) . readExpr . snd]

testToPY :: LokiTest
testToPY = (,) <$> ["helpers", "specials"]
               <*> [return . concat . fmap (PY.toPY 0) . U.catch
                   . liftM (PY.translate <$>) . readExpr . snd]

testExecPY :: LokiTest
testExecPY = (,) <$> ["simple", "complex"]
                 <*> [execPY]
    where execPY (file, lisp) = do
            py <- PY.formatPy . fmap (PY.toPY 0) . U.catch
                  . liftM (PY.translate <$>) . readExpr $ lisp
            let outFile = "tests/" ++ file ++ ".out.py"
            writeFile outFile py
            let pyOutput = SH.inproc (T.pack "python")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            SH.fold pyOutput (T.unpack <$> F.mconcat)

runTest :: LokiTest -> [IO T.Test]
runTest lt = let (files, f:_) = unzip lt
             in buildTest f <$> files
    where
        buildTest :: ((String, String) -> IO String) -> String -> IO T.Test
        buildTest f file = do
            fileContents <- readFile ("tests/" ++ file ++ ".loki")
            contents <- f (file, fileContents)
            expected <- readFile $ "tests/" ++ file ++ ".py.expected"
            let clean = filter (/= '\n')
            let contents' = clean contents
                expected' = clean expected
            return $ testCase file (expected' @=? contents')

readExpr :: String -> Either U.CompilerError [U.LokiVal]
readExpr input = case runParser P.parseExpr "py" "lisp-py" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
