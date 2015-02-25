module Utils where

import Text.Parsec
import Control.Monad.Except
import Control.Applicative hiding (Const)

import System.Info (os)
import Data.Map as M

type MetaData = M.Map String String
-- TODO: Change to record syntax, so getMetaData is cleaner?
data LokiVal = Atom MetaData String
             | List MetaData [LokiVal]
             | Number MetaData Integer
             | String MetaData String
             | Bool MetaData Bool
             | Def MetaData String LokiVal
             | Fn MetaData [String] [LokiVal]
             | Map MetaData [String] [LokiVal]
             | New MetaData String [LokiVal]
             | Dot MetaData String LokiVal [LokiVal]
             | DefClass MetaData String LokiVal [LokiVal] [LokiVal]
             | Const MetaData [String] LokiVal
             | Classfn MetaData String [String] LokiVal
             | Classvar MetaData String LokiVal
             deriving (Eq, Show)

getMetaData :: LokiVal -> MetaData
getMetaData x = case x of
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

getJsExecProgName :: String
getJsExecProgName = if os == "mingw32"
                        then "cscript"
                        else "jsc"
