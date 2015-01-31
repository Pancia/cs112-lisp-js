module Main where

import System.Environment
import System.Process

import Control.Monad

import LispJs

main :: IO ()
main = do args <- getArgs
          let expr = readExpr (head args)
          putStrLn . ("show: " ++) . show . extractValue . liftM show $ expr
          let js = catch . liftM lisp2js $ expr
          writeFile "out.js" $ js ++ "\n"
          putStrLn $ "out.js: " ++ show js
          void $ waitForProcess =<< runCommand "jsc out.js"

