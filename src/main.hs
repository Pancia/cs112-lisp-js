module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.IO
import System.Process

import Text.Parsec (parse)
import Control.Monad.Except (throwError)

import qualified LispJs as L
import qualified Utils as U
import qualified Parser as P

main :: IO ()
main = do args <- getArgs
          expr <- if head args == "f"
                      then liftM readExpr $ readFile (args !! 1)
                      else return $ readExpr (head args)
          let out = "out.js"
          --Show lisp after parsing
          putStrLn . ("show: " ++) . show . U.catch . liftM show $ expr
          putStrLn . take 60 $ repeat '#'
          --Convert to js, write to out, and print
          let jsVals = liftM (L.translate <$>) expr
          print . ("jsVal: " ++) . show $ jsVals
          putStrLn . take 60 $ repeat '#'
          js <- L.formatJs . U.catch . liftM (L.toJS <$>) $ jsVals
          print . ("js: " ++) $ js
          putStrLn . take 60 $ repeat '#'
          writeFile out js
          putStrLn $ out ++ ": " ++ show js
          --Execute js, print its result
          jsOutput <- execJS out
          print $ "jsOutput: " ++ jsOutput

execJS :: String -> IO String
execJS file = do (_, Just hout, _, _) <- createProcess $ (proc "jsc" [file]) { std_out = CreatePipe }
                 hGetContents hout

readExpr :: String -> Either U.CompilerError [L.LispVal]
readExpr input = case parse P.parseExpr "lisp-js" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
