module LispJs where

import Control.Applicative hiding (many, (<|>), Const)
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)
import Text.Printf (printf)

import Utils

primitives :: M.Map String String
primitives = M.fromList $ fmap addLokiPrefix $
        [("+", "plus"),("-", "minus"),("*", "mult"),("/", "div"),("=", "eq")
        ,("!=", "neq"),("<", "lt"),("<=", "lte"),(">", "gt"),(">=", "gte")]
        ++ (dupl <$> ["print","mod","assoc","set","range","get"])
    where
        addLokiPrefix (q,s) = (q,"loki." ++ s)
        dupl x = (x,x)

type SpecialForm = [JsVal] -> String
specialForms :: M.Map String SpecialForm
specialForms = M.fromList [("if", if_)]
    where
        if_ :: SpecialForm
        if_ [cond_, then_, else_] = "(" ++ toJS cond_ ++ " ? " ++ toJS then_ ++ " : " ++ toJS else_ ++ ")"
        if_ [cond_, then_] = if_ [cond_, then_, JsId "null"]
        if_ _ = error "wrong args to if"

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

lookupSpecForm :: String -> Maybe SpecialForm
lookupSpecForm s = M.lookup s specialForms

formatJs :: [String] -> IO String
formatJs js = do helperFns <- readFile "src/helperFunctions.js"
                 let js' = (++ ";") . L.intercalate ";\n" $ js
                 return $ helperFns ++ js'

data JsVal = JsVar String JsVal                      -- var x = ..
           | JsFn [String] [JsVal]                   -- function(..){..}
           | JsStr String                            -- ".."
           | JsBool Bool                             -- true|false
           | JsNum Integer                           -- ..-1,0,1..
           | JsId String                             -- x, foo, ..
           | JsMap [String] [JsVal]                  -- {x : y, ..}
           | JsFnCall String [JsVal]                 -- foo(..)
           | JsNewObj String [JsVal]                 -- new Foo (..)
           | JsDefClass String JsVal [JsVal] [JsVal] -- function Class(..) {..}
           | JsConst [String] JsVal                  -- Class(..) {..}
           | JsClassFn String [String] JsVal         -- Class.prototype.fn = function(..){..}
           | JsClassVar String JsVal                 -- Class(..) {this.var = val}
           | JsDotThing String JsVal [JsVal]         -- .function objname parameters*
           | JsList [JsVal]                          -- [] | [x,..]
           | JsPleaseIgnore
           deriving (Eq, Show)

translate :: LokiVal -> JsVal
translate v = if read (fromJust (M.lookup "fileType" (getMeta v))) /= JS
                  then JsPleaseIgnore
                  else case v of
                           (PleaseIgnore _) -> JsPleaseIgnore
                           (Atom _ a) -> JsId a
                           (Bool _ b) -> JsBool b
                           (Def _ n b) -> JsVar n (translate b)
                           (Fn _ xs b) -> JsFn xs (translate <$> b)
                           l@(List {}) -> list2jsVal l
                           (Number _ n) -> JsNum n
                           (String _ s) -> JsStr s
                           (New _ s l) -> JsNewObj s (translate <$> l)
                           (DefClass _ n c lf lv) -> JsDefClass n (translate c) (translate <$> lf) (translate <$> lv)
                           (Const _ s b) -> JsConst s (translate b)
                           (Classfn _ s p b) -> JsClassFn s p (translate b)
                           (Classvar _ s b) -> JsClassVar s (translate b)
                           (Dot _ fp on ps) -> JsDotThing fp (translate on) (translate <$> ps)
                           (Map _ ks vs) -> JsMap ks (translate <$> vs)
    where
        list2jsVal :: LokiVal -> JsVal
        list2jsVal l = case l of
                        (List _ [Atom _ "quote", ql]) -> translate ql
                        (List _ [Atom _ a]) -> JsFnCall a []
                        (List _ (Atom _ a:(arg:args))) -> JsFnCall a $ translate <$> (arg:args)
                        (List _ xs) -> JsList $ translate <$> xs
                        x -> catch . throwError $ TypeMismatch "List" $ show x

-- TODO: Change to ... -> Maybe String,
-- so that JsPleaseIgnore can be ignored
toJS :: JsVal -> String
toJS jv = case jv of
              a@(JsId{})       -> id2js a
              (JsNum n)        -> show n
              (JsStr s)        -> "\"" ++ s ++ "\""
              (JsBool x)       -> toLower <$> show x
              l@(JsList{})     -> list2js l
              v@(JsVar{})      -> var2js v
              f@(JsFn{})       -> fn2js f
              f@(JsFnCall{})   -> fnCall2js f
              d@(JsDotThing{}) -> dot2js d
              d@(JsDefClass{}) -> defclass2js d
              m@(JsMap{})      -> map2js m
              JsPleaseIgnore   -> ""
              x -> error "JsVal=(" ++ show x ++ ") should not be toJS'ed"

map2js :: JsVal -> String
map2js (JsMap ks vs) = "{" ++ kvs ++ "}"
    where kvs = L.intercalate ", " $ zipWith (\k v -> k ++ " : " ++ toJS v) ks vs
map2js x = catch . throwError . TypeMismatch "JsMap" $ show x

defclass2js :: JsVal -> String
defclass2js (JsDefClass name (JsConst args ret) fns vars) =
        printf "function %s(%s) {\n%s;\n%s\n};\n%s"
        name params (classVars2js vars) (ret2js ret) (fns2js fns)
    where params = L.intercalate ", " args
          ret2js :: JsVal -> String
          ret2js (JsMap ks vs) = L.intercalate ";\n" $ zipWith (\k v -> "this." ++ k ++ " = " ++ toJS v) ks vs
          ret2js x = error . show $ x
          classVars2js :: [JsVal] -> String
          classVars2js = L.intercalate ";\n" . map (\(JsClassVar s b) -> "this." ++ s ++ " = " ++ toJS b)
          fn2js' :: JsVal -> String
          fn2js' (JsClassFn fn pms ob) =
              printf "%s.prototype.%s = function(%s) {\n return %s"
              name fn (L.intercalate ", " pms) (toJS ob)
          fn2js' x = catch . throwError . TypeMismatch "JsClassFn" $ show x
          fns2js :: [JsVal] -> String
          fns2js [] = []
          fns2js l = (++ "\n};") . L.intercalate "\n};\n" $ map fn2js' l
defclass2js x = catch . throwError . TypeMismatch "JsDefClass" $ show x

fnCall2js :: JsVal -> String
fnCall2js (JsFnCall fn args)
        | isJust specForm = fromJust specForm args
        | otherwise = lookupFn fn ++ "(" ++ args' ++ ")"
    where args' = L.intercalate ", " . filter (/= "") $ toJS <$> args
          specForm = lookupSpecForm fn
fnCall2js x = catch . throwError . TypeMismatch "JsFnCall" $ show x

dot2js :: JsVal -> String
dot2js (JsDotThing fp on ps)
        | ps /= [] = printf "%s.%s(%s)" (toJS on) fp args'
        | otherwise = printf "%s.%s" (toJS on) fp
     where args' = L.intercalate ", " . filter (/= "") $ toJS <$> ps
dot2js x = catch . throwError . TypeMismatch "JsDotThing" $ show x

id2js :: JsVal -> String
id2js (JsId a) = a
id2js x = catch . throwError . TypeMismatch "JsId" $ show x

fn2js :: JsVal -> String
fn2js (JsFn params body) = printf "function (%s) {\n%s\n}" params' (showBody body)
    where params' = L.intercalate ", " params
          showBody [] = []
          showBody (b:q:bs) = toJS b ++ ";\n" ++ showBody (q:bs)
          showBody [b] = "return " ++ toJS b
fn2js x = catch . throwError . TypeMismatch "JsFn" $ show x

var2js :: JsVal -> String
var2js (JsVar name JsPleaseIgnore) = printf "var %s" name
var2js (JsVar name body) = printf "var %s = %s" name (toJS body)
var2js x = catch . throwError . TypeMismatch "JsVar" $ show x

list2js :: JsVal -> String
list2js l = case l of
               (JsList [JsId "quote", ql]) -> toJS ql
               (JsList xs) -> "[" ++ L.intercalate ", " (fmap toJS xs) ++ "]"
               x -> catch . throwError . TypeMismatch "JsList" $ show x
