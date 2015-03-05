module LokiPY where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Text.Printf (printf)

import Utils

formatPy :: [String] -> IO String
formatPy py = do helperFns <- readFile "src/helperFunctions.py"
                 let py' = L.intercalate "\n" py
                 return $ helperFns ++ py'

primitives :: M.Map String String
primitives = M.fromList $ fmap addLokiPrefix $
        [("+", "plus"),("-", "minus"),("*", "mult"),("/", "div"),("=", "eq")
        ,("!=", "neq"),("<", "lt"),("<=", "lte"),(">", "gt"),(">=", "gte")
        ,("print", "printf"),("and", "and_"),("or", "or_")]
        ++ (dupl <$> ["mod","assoc","set","range","get"])
    where
        addLokiPrefix (q,s) = (q,"Loki." ++ s)
        dupl x = (x,x)

keywords :: M.Map String String
keywords = M.fromList [("this", "self")]

type SpecialForm = [PyVal] -> String
specialForms :: M.Map String SpecialForm
specialForms = M.fromList [("if", if_),("set", set),("setp", setp),("for", for_)]
    where
        setp :: SpecialForm
        setp [PyId var, PyId prop, val] = let val' = toPY 0 val
                                          in printf "%s.%s = %s" var prop val'
        setp _ = error "wrong args to setp"
        set :: SpecialForm
        set [PyId var, val] = let val' = toPY 0 val
                              in printf "%s = %s" var val'
        set x = error $ (show x ?> "set-x") ++ "wrong args to set"
        if_ :: SpecialForm
        if_ [cond_, then_, else_] = do
            let cond_' = toPY 0 cond_
                then_' = toPY 0 then_
                else_' = toPY 0 else_
            printf "%s if %s else %s" then_' cond_' else_'
        if_ [cond_, then_] = if_ [cond_, then_, PyId "None"]
        if_ _ = error "wrong args to if"
        for_ :: SpecialForm
        for_ _ = error "wrong args to for"

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

lookupKeyword :: String -> String
lookupKeyword k = fromMaybe k $ M.lookup k keywords

lookupSpecForm :: String -> Maybe SpecialForm
lookupSpecForm s = M.lookup s specialForms

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
                      (Atom _ a)               -> PyId a
                      (Keyword _ k)            -> PyStr k --TODO: Could be wrong
                      (Number _ n)             -> PyNum n
                      (String _ s)             -> PyStr s
                      (Bool _ b)               -> PyBool b
                      (Def _ n (LkiNothing{})) -> PyVar n (PyId "None")
                      (Def _ n b)              -> PyVar n (translate b)
                      (Fn _ xs b)              -> PyFn xs (translate <$> b)
                      (New _ s l)              -> PyNewObj s (translate <$> l)
                      (DefClass _ n c lf lv)   -> PyDefClass n (translate c) (translate <$> lf) (translate <$> lv)
                      (Constr _ s b)           -> PyConst s (translateProp <$> b)
                      (Classfn _ s p b)        -> PyClassFn s p (translate b)
                      (Classvar _ s b)         -> PyClassVar s (translate b)
                      l@(List{})               -> list2pyVal l
                      (Dot _ fp on ps)         -> PyObjCall fp (translate on) (translate <$> ps)
                      (Map _ ks vs)            -> PyMap ks (translate <$> vs)
                      (LkiNothing _)           -> PyThing ""
      where
          translateProp :: (String, LokiVal) -> (String, PyVal)
          translateProp (s, l) = (s, translate l)
          list2pyVal :: LokiVal -> PyVal
          list2pyVal l = case l of
                             (List _ (Atom _ a:args)) -> PyFnCall a $ translate <$> args
                             (List _ (f@(Fn{}):args)) -> PyFnCall (toPY 0 (translate f)) $ translate <$> args
                             (List _ xs) -> PyList $ translate <$> xs
                             x -> catch . throwError . TypeMismatch "List" $ show x

toPY :: Int -> PyVal -> String
toPY n pv = case pv of
              a@(PyId{})            -> id2py n a
              (PyNum n1)            -> show n1
              (PyStr s)             -> "\"" ++ s ++ "\""
              (PyBool b)            -> show b
              l@(PyList{})          -> list2py n l
              (PyVar n1 b@(PyFn{})) -> fn2py n n1 b
              (PyVar n1 b)          -> n1 ++ " = " ++ toPY n b
              f@(PyFn{})            -> lfn2py n f
              o@(PyNewObj{})        -> new2py n o
              d@(PyObjCall{})       -> dot2py n d
              d@(PyDefClass{})      -> defclass2py n d
              m@(PyMap{})           -> map2py n m
              PyPleaseIgnore        -> ""
              f@(PyFnCall{})        -> fnCall2py n f
              x -> error $ "PyVal=(" ++ show x ++ ") should not be toPY'ed"

fnCall2py :: Int -> PyVal -> String
fnCall2py n (PyFnCall fn args)
          | isJust specForm = fromJust $ specForm <*> Just args
          | otherwise       = printf "%s(%s)" (lookupFn fn) args'
    where args' = L.intercalate ", " . filter (/= "") $ toPY n <$> args
          specForm = lookupSpecForm fn
fnCall2py _ x = catch . throwError . TypeMismatch "PyFnCall" $ show x

map2py :: Int -> PyVal -> String
map2py n (PyMap ks vs) = printf "{%s}" kvs
    where kvs = L.intercalate ", " $ zipWith (\k v -> k ++ " : " ++ toPY n v) ks vs
map2py _ x = catch . throwError . TypeMismatch "PyMap" $ show x

new2py :: Int -> PyVal -> String
new2py n (PyNewObj className args) = printf "%s(%s)" className args'
  where args' = L.intercalate ", " $ toPY n <$> args
new2py _ x = catch . throwError . TypeMismatch "PyNewObj" $ show x

defclass2py :: Int -> PyVal -> String
defclass2py n (PyDefClass name (PyConst args cbody) fs vars) =
                printf "class %s:\n%sdef __init__(self%s):\n%s%s%s"
                name (addSpacing (n + 1)) args' addCons addVars (fns2py (n + 1) fs)
            where
              args' = if null args
                        then ""
                        else ", " ++ L.intercalate ", " args
              body2py :: Int -> [(String, PyVal)] -> [String]
              body2py n' = fmap (\(p,v) -> printf (addSpacing n' ++ "self.%s = %s\n") p (toPY 0 v))
              addCons = (++ "\n") . concat $ body2py (n + 2) cbody
              addVars = vars2py (n + 1) vars
              vars2py :: Int -> [PyVal] -> String
              vars2py n' = concat . fmap (\(PyClassVar varName x) ->
                printf (addSpacing n' ++ "%s = %s\n") varName (toPY 0 x))
              fn2py_ :: Int -> PyVal -> String
              fn2py_ n' (PyClassFn fn pms body) =
                  printf "%sdef %s (%s): \n" (addSpacing n') fn (L.intercalate ", " ("self":pms)) ++
                  printf "%sreturn %s \n" (addSpacing (n'+ 1)) (toPY n' body)
              fn2py_ _ x = catch . throwError . TypeMismatch "PyClassFn" $ show x
              fns2py :: Int -> [PyVal] -> String
              fns2py _ [] = []
              fns2py n' l = (++ "\n") . L.intercalate "\n" $ map (fn2py_ n') l
defclass2py _ x = catch . throwError . TypeMismatch "PyDefClass" $ show x

list2py :: Int -> PyVal -> String
list2py n l = case l of
              (PyList [PyId "quote", ql]) -> toPY n ql
              (PyList xs) -> printf "[%s]" (L.intercalate ", " (fmap (toPY n) xs))
              x -> catch . throwError . TypeMismatch "PyList" $ show x

id2py :: Int -> PyVal -> String
id2py _ (PyId pv) = lookupKeyword pv
id2py _ x = catch . throwError . TypeMismatch "PyId" $ show x

fn2py :: Int -> String -> PyVal -> String
fn2py n n1 (PyFn params body) = printf "def %s (%s):\n%s" n1 params' body'
      where
        params' = L.intercalate ", " params
        body' = addSpacing (n + 1) ++ showBody body
        showBody [] = []
        showBody (b:q:bs) = let b' = toPY n b
                                b'' = showBody (q:bs)
                            in b' ++ "\n" ++ addSpacing (n + 1) ++ b''
        showBody [b] = "return " ++ toPY n b
fn2py _ _ x = catch . throwError . TypeMismatch "PyFn" $ show x

lfn2py :: Int -> PyVal -> String
lfn2py _ (PyFn params body) = printf "(lambda %s : [%s])" params' body'
      where
        params' = L.intercalate ", " params
        body' = L.intercalate ", " $ toPY 0 <$> body
lfn2py _ x = catch .throwError . TypeMismatch "PyFn" $ show x

dot2py :: Int -> PyVal -> String
dot2py n (PyObjCall fp on ps) = printf "%s.%s(%s) if callable(%s.%s) else %s.%s"
                                      (toPY n on) fp params' (toPY n on) fp (toPY n on) fp
      where
        params' = L.intercalate ", " $ toPY n <$> ps
dot2py _ x = catch . throwError . TypeMismatch "PyObjCall" $ show x

--Use for: if, for, while, class creation, anything else. Make sure to pass
--around a weight and increment and decrement accordingly
addSpacing :: Int -> String
addSpacing weight = replicate (weight * 4) ' '
