module LispPy where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)

import Utils
import qualified LispJs as JS

formatPy :: [String] -> IO String
formatPy py = do helperFns <- readFile "helperFunctions.py"
                 let py' = L.intercalate "\n" py
                 return $ helperFns ++ py'

data PyVal = PyVar String PyVal                -- var x = ...
           | PyFn [String] [PyVal]             -- function(...){...}
           | PyStr String                      -- "..."
           | PyBool Bool                       -- true|false
           | PyNum Integer                     -- ..-1,0,1..
           | PyId String                       -- x, foo, ...
           | PyObjCall String [String] [PyVal] -- x.foo.bar(...)
           | PyFnCall String [PyVal]           -- foo(...)
           | PyList [PyVal]                    -- [...]
           | PyThing String                    -- ???
           deriving (Eq, Show)

translate :: JS.LispVal -> PyVal
translate v = case v of
                  (JS.Atom a) -> PyId a
                  (JS.Number n) -> PyNum n
                  (JS.String s) -> PyStr s
                  (JS.Bool b) -> PyBool b
                  (JS.Def n b) -> PyVar n (translate b)
                  _ -> PyThing $ show v

toPY :: PyVal -> String
toPY pv = case pv of
              a@(PyId{})      -> id2py a
              (PyNum n)       -> show n
              (PyStr s)       -> "\"" ++ s ++ "\""
              (PyBool b)      -> toLower <$> show b
              (PyVar n b)     -> n ++ " = " ++ toPY b
              _ -> show pv
             
id2py :: PyVal -> String
id2py (PyId pv) = pv
id2py x = catch . throwError . TypeMismatch "PyId" $ show x