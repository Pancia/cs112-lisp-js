module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.IO
import System.Process

import qualified LispJs as L

main :: IO ()
main = do args <- getArgs
          expr <- if head args == "f"
                      then liftM L.readExpr $ readFile (args !! 1)
                      else return $ L.readExpr (head args)
          let out = "out.js"
          --Show lisp after parsing
          putStrLn . ("show: " ++) . show . L.catch . liftM show $ expr
          putStrLn . take 60 $ repeat '#'
          --Convert to js, write to out, and print
          let jsVals = liftM (L.translate <$>) expr
          print . ("jsVal: " ++) . show $ jsVals
          putStrLn . take 60 $ repeat '#'
          js <- L.formatJs . L.catch . liftM (L.toJS <$>) $ jsVals
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
