module LispPy where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)

import Utils

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

data PyVal = PyVar String PyVal                -- x = ...
           | PyFn [String] [PyVal]             -- function(...){...}
           | PyStr String                      -- "..."
           | PyBool Bool                       -- true|false
           | PyNum Integer                     -- ..-1,0,1..
           | PyId String                       -- x, foo, ...
           | PyObjCall String String [PyVal]   -- x.foo.bar(...)
           | PyFnCall String [PyVal]           -- foo(...)
           | PyList [PyVal]                    -- [...]
           | PyThing String                    -- ???
           deriving (Eq, Show)

translate :: LispVal -> PyVal
translate v = case v of
                  (Atom a) -> PyId a
                  (Number n) -> PyNum n
                  (String s) -> PyStr s
                  (Bool b) -> PyBool b
                  (Def n b) -> PyVar n (translate b)
                  (Fn xs b) -> PyFn xs (translate <$> b)
                  l@(List _) -> list2pyVal l
                  (Dot fp on ps) -> PyObjCall fp on (translate <$> ps)
                  _ -> PyThing $ show v
      where
        list2pyVal :: LispVal -> PyVal
        list2pyVal l = case l of
                           (List (Atom a:args)) -> PyFnCall a $ translate <$> args
                           x -> catch . throwError . TypeMismatch "List" $ show x

toPY :: PyVal -> String
toPY pv = case pv of
              a@(PyId{})            -> id2py a
              (PyNum n)             -> show n
              (PyStr s)             -> "\"" ++ s ++ "\""
              (PyBool b)            -> toLower <$> show b
              (PyVar n b)           -> n ++ " = " ++ toPY b
              f@(PyFn{})            -> fn2py f
              d@(PyObjCall{})      -> dot2py d
              (PyFnCall n as)
                  | lookupFn n /= n -> lookupFn n ++ "([" ++ args ++ "])"
                  | otherwise       -> lookupFn n ++ "(" ++ args ++ ")"
                  where
                    args = L.intercalate ", " $ toPY <$> as
              _ -> show pv

id2py :: PyVal -> String
id2py (PyId pv) = pv
id2py x = catch . throwError . TypeMismatch "PyId" $ show x

fn2py :: PyVal -> String
fn2py (PyFn params body) = "(lambda " ++ params' ++ " : " ++ showBody body ++ ")"
      where
        params' = L.intercalate ", " params
        showBody [] = []
        showBody (b:q:bs) = toPY b ++ ";\n" ++ showBody (q:bs)
        showBody [b] = toPY b
fn2py x = catch . throwError . TypeMismatch "PyFn" $ show x

dot2py :: PyVal -> String
dot2py (PyObjCall fp on ps)
      | ps /= [] = on ++ "." ++ fp ++ "(" ++ params' ++ ")"
      | otherwise = on ++ "." ++ fp
      where
        params' = L.intercalate ", " $ toPY <$> ps
dot2py x = catch . throwError . TypeMismatch "PyObjCall" $ show x

--Use for: if, for, while, anything else. Make sure to pass around a weight and
--increment and decrement accordingly
addSpacing :: Int -> String
addSpacing weight = replicate (weight * 4) ' '
