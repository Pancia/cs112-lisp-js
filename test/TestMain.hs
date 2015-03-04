{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
--import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} TestLokiJS
import {-@ HTF_TESTS @-} TestLokiPY

import System.Environment

main :: IO ()
main = do args <- getArgs
          htfMainWithArgs args htf_importedTests
