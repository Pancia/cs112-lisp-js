module LokiPY where

import Control.Applicative hiding (many, (<|>), Const)
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Utils

formatPy :: [String] -> IO String
formatPy py = do helperFns <- readFile "src/helperFunctions.py"
                 let py' = L.intercalate "\n" py
                 return $ helperFns ++ py'

primitives :: M.Map String String
primitives = M.fromList $ fmap addLokiPrefix $
        [("+", "plus"),("-", "minus"),("*", "mult"),("/", "div"),("=", "eq")
        ,("!=", "neq"),("<", "lt"),("<=", "lte"),(">", "gt"),(">=", "gte")
        ,("print", "printf")]
        ++ (dupl <$> ["and","or","mod","assoc","set","range","get"])
    where
        addLokiPrefix (q,s) = (q,"Loki." ++ s)
        dupl x = (x,x)

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

data PyVal = PyVar String PyVal                -- x = ...
           | PyFn [String] [PyVal]             -- function(...){...}
           | PyStr String                      -- "..."
           | PyBool Bool                       -- true|false
           | PyNum Integer                     -- ..-1,0,1..
           | PyId String                       -- x, foo, ...
           | PyMap [String] [PyVal]            -- {x : y, ..}
           | PyObjCall String PyVal [PyVal]    -- x.foo.bar(...)
           | PyFnCall String [PyVal]           -- foo(...)
           | PyNewObj String [PyVal]                 -- new Foo(..)
           | PyDefClass String PyVal [PyVal] [PyVal] -- function Class(..) {..}
           | PyConst [String] [(String, PyVal)]      -- Class(..) {..}
           | PyClassFn String [String] PyVal         -- TODO ex
           | PyClassVar String PyVal
           | PyList [PyVal]                    -- [...]
           | PyPleaseIgnore -- ignore
           | PyThing String -- ???
           deriving (Eq, Show)

translate :: LokiVal -> PyVal
translate v = if read (fromJust (M.lookup "fileType" (getMeta v))) /= PY
                  then PyPleaseIgnore
                  else case v of
                      (Atom _ a)             -> PyId a
                      (Number _ n)           -> PyNum n
                      (String _ s)           -> PyStr s
                      (Bool _ b)             -> PyBool b
                      (Def _ n b)            -> PyVar n (translate b)
                      (Fn _ xs b)            -> PyFn xs (translate <$> b)
                      (New _ s l)            -> PyNewObj s (translate <$> l)
                      (DefClass _ n c lf lv) -> PyDefClass n (translate c) (translate <$> lf) (translate <$> lv)
                      (Const _ s b)          -> PyConst s (translateProp <$> b)
                      (Classfn _ s p b)      -> PyClassFn s p (translate b)
                      (Classvar _ s b)       -> PyClassVar s (translate b)
                      l@(List{})             -> list2pyVal l
                      (Dot _ fp on ps)       -> PyObjCall fp (translate on) (translate <$> ps)
                      (Map _ ks vs)          -> PyMap ks (translate <$> vs)
                      _                      -> PyThing $ show v
      where
          translateProp :: (String, LokiVal) -> (String, PyVal)
          translateProp (s, l) = (s, translate l)
          list2pyVal :: LokiVal -> PyVal
          list2pyVal l = case l of
                             (List _ (Atom _ a:args)) -> PyFnCall a $ translate <$> args
                             (List _ xs) -> PyList $ translate <$> xs
                             x -> catch . throwError . TypeMismatch "List" $ show x

toPY :: Int -> PyVal -> String
toPY n pv = case pv of
              a@(PyId{})       -> id2py n a
              (PyNum n1)       -> show n1
              (PyStr s)        -> "\"" ++ s ++ "\""
              (PyBool b)       -> show b
              l@(PyList{})     -> list2py n l
              (PyVar n1 b)     -> n1 ++ " = " ++ toPY n b
              f@(PyFn{})       -> fn2py n f
              o@(PyNewObj{})   -> new2py n o
              d@(PyObjCall{})  -> dot2py n d
              d@(PyDefClass{}) -> defclass2py n d
              m@(PyMap{})      -> map2py n m
              PyPleaseIgnore   -> ""
              f@(PyFnCall{})   -> fnCall2py n f
              x -> error "PyVal=(" ++ show x ++ ") should not be toPY'ed"

fnCall2py :: Int -> PyVal -> String
fnCall2py n (PyFnCall fn args) = lookupFn fn ++ "(" ++ args' ++ ")"
    where args' = L.intercalate ", " . filter (/= "") $ toPY n <$> args
fnCall2py _ x = catch . throwError . TypeMismatch "JsFnCall" $ show x

map2py :: Int -> PyVal -> String
map2py n (PyMap ks vs) = "{" ++ kvs ++ "}"
    where kvs = L.intercalate ", " $ zipWith (\k v -> k ++ " : " ++ toPY n v) ks vs
map2py _ x = catch . throwError . TypeMismatch "PyMap" $ show x

new2py :: Int -> PyVal -> String
new2py n (PyNewObj className args) = className ++ "(" ++ args' ++ ")"
  where args' = L.intercalate ", " $ toPY n <$> args
new2py _ x = catch . throwError . TypeMismatch "PyNewObj" $ show x

defclass2py :: Int -> PyVal -> String
defclass2py n (PyDefClass name (PyConst args body) _ vars) = "class " ++ name ++ ":\n"
                ++ addSpacing (n + 1) ++ "def __init__(self" ++ args' ++ "):\n" ++
                addCons ++ addVars
                where
                  args' = if null args
                            then ""
                            else ", " ++ L.intercalate ", " args
                  body2py :: Int -> [(String, PyVal)] -> [String]
                  body2py n' = fmap (\(p,v) -> p ++ toPY (n' + 2) v)
                  addCons = if show body /= "{}"
                              then L.intercalate ("\n" ++ addSpacing (n + 2)) $ body2py (n + 2) body
                              else ""
                  addVars = L.intercalate ("\n " ++ addSpacing 1) $ toPY n <$> vars
defclass2py _ x = catch . throwError . TypeMismatch "PyDefClass" $ show x

list2py :: Int -> PyVal -> String
list2py n l = case l of
              (PyList [PyId "quote", ql]) -> toPY n ql
              (PyList xs) -> "[" ++ L.intercalate ", " (fmap (toPY n) xs) ++ "]"
              x -> catch . throwError . TypeMismatch "PyList" $ show x

id2py :: Int -> PyVal -> String
id2py _ (PyId pv) = pv
id2py _ x = catch . throwError . TypeMismatch "PyId" $ show x

fn2py :: Int -> PyVal -> String
fn2py n (PyFn params body) = "(lambda " ++ params' ++ " : " ++ showBody body ++ ")"
      where
        params' = L.intercalate ", " params
        showBody [] = []
        showBody (b:q:bs) = toPY n b ++ "\n" ++ showBody (q:bs)
        showBody [b] = toPY n b
fn2py _ x = catch . throwError . TypeMismatch "PyFn" $ show x

dot2py :: Int -> PyVal -> String
dot2py n (PyObjCall fp on ps)
      | ps /= [] = toPY n on ++ "." ++ fp ++ "(" ++ params' ++ ")"
      | otherwise = toPY n on ++ "." ++ fp
      where
        params' = L.intercalate ", " $ toPY n <$> ps
dot2py _ x = catch . throwError . TypeMismatch "PyObjCall" $ show x

--Use for: if, for, while, anything else. Make sure to pass around a weight and
--increment and decrement accordingly
addSpacing :: Int -> String
addSpacing weight = replicate (weight * 4) ' '
