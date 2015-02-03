module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO
import System.Process

import qualified LispJs as L

main :: IO ()
main = do args <- getArgs
          let expr = L.readExpr (head args)
          putStrLn . ("show: " ++) . show . L.catch . liftM show $ expr
          let js = L.catch . liftM (map L.lisp2js) $ expr
          helperFns <- readFile "helperFunctions.js"
          writeFile "out.js" $ helperFns ++ "\n" ++ intercalate ";\n" js ++ ";"
          putStrLn $ "out.js: " ++ show js
          (_, Just hout, _, _) <- createProcess $ (proc "jsc" ["out.js"]) { std_out = CreatePipe }
          jsOutput <- hGetContents hout
          print $ "jsOutput: " ++ jsOutput
