module Utils where

import Text.Parsec
import Control.Monad.Except
import Control.Applicative

import System.Info (os)

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
