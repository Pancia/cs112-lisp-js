module TestUtils where

import Control.Applicative hiding (Const)
import Control.Monad.Except (throwError)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Parsec (runParser)
import Text.Printf (printf)
import Utils (LokiVal)

import qualified Parser as P
import qualified Test.Framework as T
import qualified Utils as U

type LokiTestFn = (String, String) -> IO (String, String)
type LokiTests = [(String, LokiTestFn)]

runTest :: String -> LokiTests -> [IO T.Test]
runTest type_ lt = let (files, f:_) = unzip lt
                   in buildTest f <$> files
    where
        buildTest :: LokiTestFn -> String -> IO T.Test
        buildTest f file = do
            fileContents <- readFile ("tests/" ++ file ++ ".loki")
            (testType, contents) <- f (file, fileContents)
            let testName = printf "%s.%s.%s" file testType type_
            expected <- readFile $ printf "tests/%s.expected" testName
            let clean = filter (/= '\n')
                contents' = clean contents
                expected' = clean expected
            return $ testCase testName (expected' @=? contents')

readExpr :: String -> String -> Either U.CompilerError [LokiVal]
readExpr type_ input = case runParser P.parseExpr type_ ("loki-" ++ type_) input of
                           Left err -> throwError $ U.ParserErr err
                           Right val -> return val
