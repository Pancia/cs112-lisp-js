module TestUtils where

import Control.Monad.Except (throwError)
import Text.Parsec (runParser)
import Text.Printf (printf)
import Utils (LokiVal)

import qualified Parser as P
import qualified Utils as U

readExpr :: String -> String -> String -> Either U.CompilerError [LokiVal]
readExpr type_ tag input = case runParser P.parseExpr type_ tag' input of
                               Left err -> throwError $ U.ParserErr err
                               Right val -> return val
    where
        tag' = "loki-" ++ type_ ++ "-" ++ tag

getInFile :: String -> String
getInFile testName = printf "tests/%s.loki" testName

getXpFile :: String -> String -> String -> String
getXpFile testName testType fileType =
        printf "tests/%s.%s.%s.expected" testName testType fileType

clean :: String -> String
clean = filter (/= '\n')
