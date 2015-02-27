module Utils where

import Text.Parsec
import Control.Monad.Except
import Control.Applicative hiding (Const)

import System.Info (os)
import Data.Map as M
import Data.Char (toLower)

import Debug.Trace

(?>) :: (Show a) => a -> String -> a
(?>) a s = trace (s ++ show a) a

type Meta = M.Map String String
-- TODO: Change to record syntax, so [get|set]Meta is cleaner?
data LokiVal = Atom Meta String
             | List Meta [LokiVal]
             | Number Meta Integer
             | String Meta String
             | Bool Meta Bool
             | Def Meta String LokiVal
             | Fn Meta [String] [LokiVal]
             | Map Meta [String] [LokiVal]
             | New Meta String [LokiVal]
             | Dot Meta String LokiVal [LokiVal]
             | DefClass Meta String LokiVal [LokiVal] [LokiVal]
             | Const Meta [String] [(String, LokiVal)]
             | Classfn Meta String [String] LokiVal
             | Classvar Meta String LokiVal
             | PleaseIgnore Meta
             deriving (Eq, Show)

getMeta :: LokiVal -> Meta
getMeta x = case x of
                Atom m _ -> m
                List m _ -> m
                Number m _ -> m
                String m _ -> m
                Bool m _ -> m
                Def m _ _ -> m
                Fn m _ _ -> m
                Map m _ _ -> m
                New m _ _ -> m
                Dot m _ _ _ -> m
                DefClass m _ _ _ _ -> m
                Const m _ _ -> m
                Classfn m _ _ _ -> m
                Classvar m _ _ -> m
                PleaseIgnore m -> m

setMeta :: Meta -> LokiVal -> LokiVal
setMeta m x = case x of
                  Atom _ a -> Atom m a
                  List _ a -> List m a
                  Number _ a -> Number m a
                  String _ a -> String m a
                  Bool _ a -> Bool m a
                  Def _ a b -> Def m a b
                  Fn _ a b -> Fn m a b
                  Map _ a b -> Map m a b
                  New _ a b -> New m a b
                  Dot _ a b c -> Dot m a b c
                  DefClass _ a b c d -> DefClass m a b c d
                  Const _ a b -> Const m a b
                  Classfn _ a b c -> Classfn m a b c
                  Classvar _ a b -> Classvar m a b
                  PleaseIgnore _ -> PleaseIgnore m

data OutputType = JS | PY
                deriving (Eq, Show)

instance Read OutputType where
        readsPrec _ s = case toLower <$> s of
                            "js" -> [(JS,"")]
                            "py" -> [(PY,"")]
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
catch (Right val) = val
catch (Left err) = error . show $ err

caseWindowsOrOther :: a -> a -> a
caseWindowsOrOther windows other = if os == "mingw32" then windows else other
