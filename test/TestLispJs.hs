module TestLispJs where

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

import qualified LispJs as JS
import qualified Utils as U
import qualified Parser as P

type LokiTest = [(String, (String, String) -> IO String)]

tests :: IO [T.Test]
tests = sequence . join $ runTest <$> [testJsParser, testToJS, testExecJS]

testJsParser :: LokiTest
testJsParser = (,) <$> ["sexp"] <*> [return . U.catch . (show <$>) . readExpr . snd]

testToJS :: LokiTest
testToJS = (,) <$> ["helpers", "specials"]
               <*> [return . concat . fmap JS.toJS . U.catch
                   . liftM (JS.translate <$>) . readExpr . snd]

testExecJS :: LokiTest
testExecJS = (,) <$> ["simple", "complex"] <*> [execJS]
    where execJS (file, lisp) = do
            js <- JS.formatJs . fmap JS.toJS . U.catch
                  . liftM (JS.translate <$>) . readExpr $ lisp
            let outFile = "tests/" ++ file ++ ".out.js"
            writeFile outFile js
            let jsOutput = SH.inproc (T.pack $ U.caseWindowsOrOther "cscript" "jsc")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            SH.fold jsOutput (T.unpack <$> F.mconcat)

runTest :: LokiTest -> [IO T.Test]
runTest lt = let (files, f:_) = unzip lt
             in buildTest f <$> files
    where
        buildTest :: ((String, String) -> IO String) -> String -> IO T.Test
        buildTest f file = do
            fileContents <- readFile ("tests/" ++ file ++ ".loki")
            contents <- f (file, fileContents)
            expected <- readFile $ "tests/" ++ file ++ ".js.expected"
            let clean = filter (/= '\n')
            let contents' = clean contents
                expected' = clean expected
            return $ testCase file (expected' @=? contents')

readExpr :: String -> Either U.CompilerError [U.LokiVal]
readExpr input = case runParser P.parseExpr "js" "lisp-js" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
