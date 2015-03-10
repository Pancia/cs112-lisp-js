{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestLokiPY where

import Control.Applicative hiding (Const)
import Control.Monad

import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified LokiPY as PY
import qualified Test.Framework as TF
import qualified Turtle as SH
import qualified TestUtils as TU
import qualified Utils as U

test_toPY :: IO ()
test_toPY = void $ mapM testToPY ["def+fn","class-object","helpers"
                                 ,"primitives","specials", "literals"]
    where
        testToPY testName = do
            lisp <- liftM (TU.readExpr "py") $ readFile inFile
            let translated'' = U.catch . liftM (PY.translate <$>) $ lisp
                translated' = concat . fmap (PY.toPY 0) $ translated''
                translated = TU.clean translated'
            expected <- liftM TU.clean $ readFile xpFile
            TF.assertEqual_ (TF.makeLoc xpFile 0) expected translated
          where
              inFile = TU.getInFile testName
              xpFile = TU.getXpFile testName "convert" "py"

test_execPY :: IO ()
test_execPY = void $ mapM testExecPY ["def+fn","class-object","helpers"
                                     ,"literals","primitives","specials"]
    where
        testExecPY testName = do
            lisp <- liftM (TU.readExpr "py") $ readFile inFile
            py <- PY.formatPy . fmap (PY.toPY 0) . U.catch
                  . liftM (PY.translate <$>) $ lisp
            writeFile outFile py
            let pyOutput = SH.inproc (T.pack "python")
                                     [FS.encode $ FS.decodeString outFile]
                                     SH.empty
            executed <- liftM TU.clean $ SH.fold pyOutput (T.unpack <$> F.mconcat)
            expected <- liftM TU.clean $ readFile xpFile
            TF.assertEqual_ (TF.makeLoc xpFile 0) expected executed
          where
              inFile = TU.getInFile testName
              xpFile = TU.getXpFile testName "exec" "py"
              outFile = "tests/" ++ testName ++ ".out.py"
