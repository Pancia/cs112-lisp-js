module Main where

import Control.Monad
import System.Environment
import System.IO
import System.Process

import qualified LispJs as L

main :: IO ()
main = do args <- getArgs
          let out = "out.js"
              expr = L.readExpr (head args)
          --Show lisp after parsing
          putStrLn . ("show: " ++) . show . L.catch . liftM show $ expr
          --Convert to js, write to out, and print
          js <- L.formatJs . L.catch . liftM (map L.lisp2js) $ expr
          writeFile out js
          putStrLn $ out ++ ": " ++ show js
          --Execute js, print its result
          jsOutput <- execJs out
          print $ "jsOutput: " ++ jsOutput

execJs :: String -> IO String
execJs file = do (_, Just hout, _, _) <- createProcess $ (proc "jsc" [file]) { std_out = CreatePipe }
                 hGetContents hout
