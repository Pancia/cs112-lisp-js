{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
--import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} TestLokiJS
import {-@ HTF_TESTS @-} TestLokiPY

main :: IO ()
main = htfMainWithArgs ["--colors=true"] htf_importedTests
