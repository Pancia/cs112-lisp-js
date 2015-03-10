module Utils where

import Text.Parsec
import Control.Monad.Except
import Control.Applicative

import System.Info (os)
import Data.Map as M
import Data.Char (toLower)

import Debug.Trace

(?>) :: (Show a) => a -> String -> a
(?>) a s = trace (s ++ show a) a

type Meta = M.Map String String
data LokiVal = Atom    { getMeta :: Meta, getAtom :: String }
             | List    { getMeta :: Meta
                       , getList :: [LokiVal] }
             | Array   { getMeta :: Meta
                       , getArray :: [LokiVal] }
             | Tuple   { getMeta :: Meta
                       , getTuple :: [LokiVal] }
             | Number  { getMeta :: Meta
                       , getNumber :: Integer }
             | String  { getMeta :: Meta
                       , getString :: String }
             | Keyword { getMeta :: Meta
                       , getKeyword :: String }
             | Bool    { getMeta :: Meta
                       , getBool :: Bool }
             | Def     { getMeta :: Meta
                       , getDefName :: String
                       , getDefBody :: LokiVal }
             | Fn      { getMeta :: Meta
                       , getFnArgs :: [String]
                       , getFnBody :: [LokiVal] }
             | Map     { getMeta :: Meta
                       , getMapKeys :: [String]
                       , getMapVals :: [LokiVal] }
             | New     { getMeta :: Meta
                       , getNewClass :: String
                       , getNewArgs :: [LokiVal] }
             | Dot     { getMeta :: Meta
                       , getDotObj :: String
                       , getDotProp :: LokiVal
                       , getDotArgs :: [LokiVal] }
             | DefClass { getMeta :: Meta
                        , getClassName :: String
                        , getSuperClasses :: [String]
                        , getClassConstr :: LokiVal
                        , getClassFns :: [LokiVal]
                        , getClassVars :: [LokiVal] }
             -- TODO: Rename to ClassConstr
             | Constr   { getMeta :: Meta
                        , getClassConstrParams :: [String]
                        , getClassConstrBody :: [(String, LokiVal)] }
             | ClassSuper { getMeta :: Meta
                          , getSuperClassName :: String
                          , getSuperArgs :: [LokiVal] }
             | Classfn  { getMeta :: Meta
                        , getClassFnName :: String
                        , getClassFnParams :: [String]
                        , getClassFnBody :: LokiVal }
             | Classvar { getMeta :: Meta
                        , getClassVarName :: String
                        , getClassVarBody :: LokiVal }
             | LkiNothing { getMeta :: Meta }
             deriving (Eq, Show)

data OutputType = JS | PY | HTML
                deriving (Eq)
instance Show OutputType where
        show t = case t of
                     JS -> "js"
                     PY -> "py"
                     HTML -> "html"
instance Read OutputType where
        readsPrec _ s = case toLower <$> s of
                            "js" -> [(JS,"")]
                            "py" -> [(PY,"")]
                            "html" -> [(HTML,"")]
                            _ -> []

data CompilerError = NumArgs        Integer [String]
                   | TypeMismatch   String String
                   | ParserErr      ParseError
                   | BadSpecialForm String String
                   | NotFunction    String String
                   | UnboundVar     String String
                   | Default        String

instance Show CompilerError where
        show = showError

showError :: CompilerError -> String
showError (UnboundVar msg var)      = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg f)       = msg ++ ": " ++ show f
showError (NumArgs expctd found) =
        "Expected " ++ show expctd ++
        " args; found values " ++ unwords (show <$> found)
showError (TypeMismatch expctd fnd) =
        "Invalid type, expected: " ++ expctd ++
        ", found: " ++ show fnd
showError (ParserErr parseErr) = "ParseErr at " ++ show parseErr
showError (Default err) = err

hError :: Either CompilerError String -> Either CompilerError String
hError = flip catchError (return . show)

catch :: Either CompilerError a -> a
catch = either (error . show) id

caseOS :: a -> a -> a
caseOS windows other = if os == "mingw32" then windows else other
