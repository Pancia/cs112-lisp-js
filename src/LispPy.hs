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

primitives :: M.Map String String
primitives = M.fromList [("+", "plus")
                        ,("-", "minus")
                        ,("*", "mult")
                        ,("/", "div")
                        ,("=", "eq")
                        ,("!=", "neq")
                        ,("<", "lt")
                        ,("<=", "lte")
                        ,(">", "gt")
                        ,(">=", "gte")
                        ,("and", "and_")
                        ,("or", "or_")]

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

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
                  l@(JS.List _) -> list2pyVal l
                  _ -> PyThing $ show v
      where
        list2pyVal :: JS.LispVal -> PyVal
        list2pyVal l = case l of
                        (JS.List (JS.Atom a:args)) -> PyFnCall a $ translate <$> args

toPY :: PyVal -> String
toPY pv = case pv of
              a@(PyId{})            -> id2py a
              (PyNum n)             -> show n
              (PyStr s)             -> "\"" ++ s ++ "\""
              (PyBool b)            -> toLower <$> show b
              (PyVar n b)           -> n ++ " = " ++ toPY b
              (PyFnCall n as)
                  | lookupFn n /= n -> lookupFn n ++ "([" ++ args ++ "])"
                  | otherwise       -> lookupFn n ++ "(" ++ args ++ ")"
                  where
                    args = L.intercalate ", " $ toPY <$> as
              _ -> show pv

id2py :: PyVal -> String
id2py (PyId pv) = pv
id2py x = catch . throwError . TypeMismatch "PyId" $ show x

--Use for: if, for, while, anything else. Make sure to pass around a weight and
--increment and decrement accordingly
addSpacing :: Int -> String
addSpacing weight = replicate (weight * 4) ' '
