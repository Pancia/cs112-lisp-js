module Main where

import System.Environment
import System.Process

import Control.Monad

import qualified LispJs as L

main :: IO ()
main = do args <- getArgs
          let expr = L.readExpr (head args)
          putStrLn . ("show: " ++) . show . L.catch . liftM show $ expr
          let js = L.catch . liftM L.lisp2js $ expr
          writeFile "out.js" $ js ++ "\n"
          putStrLn $ "out.js: " ++ show js
          void $ waitForProcess =<< runCommand "jsc out.js"

