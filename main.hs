import Text.Parsec hiding (spaces)
import System.Environment

import Data.Functor.Identity (Identity)
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool

main :: IO ()
main = do args <- getArgs
          let expr = readExpr (head args)
          print . extractValue . liftM show $ expr
          print . extractValue . trapError . liftM show $ eval =<< expr

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp-js" input of
                     Left err -> throwError $ ParserErr err
                     Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval lv = case lv of
              (List [Atom "quote", ql]) -> return ql
              (List (Atom f : args)) -> mapM eval args >>= apply f
              (Atom _)   -> return lv
              (String _) -> return lv
              (Number _) -> return lv
              (Bool _)   -> return lv
              (List [])  -> return lv
              (List _)   -> return lv

type Parser a = ParsecT String () Identity a
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> try parseString
        <|> try parseNumber
        <|> try parseQuoted
        <|> try parseList

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
                     "#t" -> Bool True
                     "#f" -> Bool False
                     _    -> Atom atom

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseQuoted :: Parser LispVal
parseQuoted = do _ <- char '\''
                 x <- parseExpr
                 let x' = case x of
                              (List [l]) -> l
                              _ -> x
                 return $ List [Atom "quote", x']

parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')')
                     (sepBy parseExpr spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

showVal :: LispVal -> String
showVal lv = case lv of
                 (String s)   -> "\"" ++ s ++ "\""
                 (Atom a)     -> a
                 (Number n)   -> show n
                 (List l)     -> "(" ++ unwordsList l ++ ")"
                 (Bool True)  -> "#t"
                 (Bool False) -> "#f"

instance Show LispVal where
        show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f xs = maybe (throwError $ NotFunction "Not args for a func" f)
                   ($ xs)
                   (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quot", numericBinop quot),
              ("rem", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op xs = mapM unpackNum xs >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = Left $ TypeMismatch "number" notNum

data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | ParserErr      ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String

type ThrowsError = Either LispError

instance Show LispError where
        show = showError

showError :: LispError -> String
showError (UnboundVar msg var)      = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg f)       = msg ++ ": " ++ show f
showError (NumArgs expctd found) =
        "Expected " ++ show expctd ++
        " args; found values " ++ unwordsList found
showError (TypeMismatch expctd fnd) =
        "Invalid type, expected: " ++ expctd ++
        ", found: " ++ show fnd
showError (ParserErr parseErr) = "ParseErr at " ++ show parseErr
showError (Default err) = err

trapError = flip catchError (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
