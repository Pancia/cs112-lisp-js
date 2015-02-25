module Utils where

import Text.Parsec
import Control.Monad.Except
import Control.Applicative hiding (Const)

import System.Info (os)
import Data.Map as M

type MetaData = M.Map String String
-- TODO: Change to record syntax, so getMetaData is cleaner?
data LispVal = Atom MetaData String
             | List MetaData [LispVal]
             | Number MetaData Integer
             | String MetaData String
             | Bool MetaData Bool
             | Def MetaData String LispVal
             | Fn MetaData [String] [LispVal]
             | Map MetaData [String] [LispVal]
             | New MetaData String [LispVal]
             | Dot MetaData String LispVal [LispVal]
             | DefClass MetaData String LispVal [LispVal] [LispVal]
             | Const MetaData [String] LispVal
             | Classfn MetaData String [String] LispVal
             | Classvar MetaData String LispVal
             deriving (Eq, Show)

getMetaData :: LispVal -> MetaData
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
