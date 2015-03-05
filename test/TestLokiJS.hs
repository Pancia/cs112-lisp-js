{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestLokiJS where

import Data.Maybe
import Control.Applicative hiding (Const)
import Control.Monad

import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified LokiJS as JS
import qualified Test.Framework as TF
import qualified Turtle as SH
import qualified TestUtils as TU
import qualified Utils as U

test_jsParser :: IO ()
test_jsParser = void $ mapM testParser ["class-object","specials","def+fn"
                                       ,"primitives","literals","helpers"]
    where
        testParser :: String -> IO ()
        testParser testName = do
            lisp <- liftM (TU.readExpr "js") $ readFile inFile
            let parsed = TU.clean . U.catch $ show <$> lisp
            expected <- liftM TU.clean $ readFile xpFile
            TF.assertEqual_ (TF.makeLoc xpFile 0) expected parsed
          where
              inFile = TU.getInFile testName
              xpFile = TU.getXpFile testName "parse" "js"

test_toJS :: IO ()
test_toJS = void $ mapM testToJS ["class-object","helpers","specials","def+fn"]
    where
        testToJS testName = do
            lisp <- liftM (TU.readExpr "js") $ readFile inFile
            let translated'' = U.catch . liftM (JS.translate <$>) $ lisp
                translated' = concat . mapMaybe JS.toJS $ translated''
                translated = TU.clean translated'
            expected <- liftM TU.clean $ readFile xpFile
            TF.assertEqual_ (TF.makeLoc xpFile 0) expected translated
          where
              inFile = TU.getInFile testName
              xpFile = TU.getXpFile testName "convert" "js"

test_execJS :: IO ()
test_execJS = void $ mapM testExecJS ["class-object","helpers", "def+fn"]
    where
        testExecJS testName = do
            lisp <- liftM (TU.readExpr "js") $ readFile inFile
            js <- JS.formatJs . fmap JS.toJS . U.catch
                  . liftM (JS.translate <$>) $ lisp
            writeFile outFile js
            let jsOutput = SH.inproc (T.pack "node")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            exec_ed <- liftM TU.clean $ SH.fold jsOutput (T.unpack <$> F.mconcat)
            expected <- liftM TU.clean $ readFile xpFile
            TF.assertEqual_ (TF.makeLoc xpFile 0) expected exec_ed
          where
              inFile = TU.getInFile testName
              xpFile = TU.getXpFile testName "exec" "js"
              outFile = "tests/" ++ testName ++ ".out.js"
