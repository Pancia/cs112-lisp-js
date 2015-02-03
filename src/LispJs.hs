module LispJs where

import Text.Parsec hiding (spaces)

import Data.Functor.Identity (Identity)
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

import qualified Data.List as L
import Data.Maybe
import qualified Data.Map as M

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Def String LispVal
             deriving (Eq)

primitives :: M.Map String String
primitives = M.fromList [("log.", "print")
                        ,("+", "plus")
                        ,("-", "minus")
                        ,("=", "eq")]

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

lisp2js :: LispVal -> String
lisp2js l = case l of
                (Atom a) -> a
                (Number n) -> show n
                (String _) -> show l
                (Bool x) -> show x
                list@(List _) -> list2js list
                def@(Def _ _) -> show def

list2js :: LispVal -> String
list2js l = case l of
                (List [Atom "quote", List ql]) -> show ql
                (List (Atom a:args)) -> lookupFn a ++ "(" ++ L.intercalate ", " (fmap lisp2js args) ++ ")"
                (List xs) -> "[" ++ L.intercalate ", " (fmap lisp2js xs) ++ "]"
                x -> catch . throwError $ TypeMismatch "List" x

readExpr :: String -> Either LispError [LispVal]
readExpr input = case parse parseExpr "lisp-js" input of
                     Left err -> throwError $ ParserErr err
                     Right val -> return val

type Parser a = ParsecT String () Identity a
parseExpr :: Parser [LispVal]
parseExpr = many1 $ try (parseExpr1 <* skipMany space)

parseExpr1 :: Parser LispVal
parseExpr1 = parseAtom
             <|> try parseString
             <|> try parseNumber
             <|> try parseQuoted
             <|> try parseDef
             <|> try parseList

parseDef :: Parser LispVal
parseDef = between (char '(') (char ')') $ do
        _ <- string "def"
        spaces
        name <- many1 (letter <|> digit <|> symbol) <* spaces
        body <- parseExpr1
        return $ Def name body

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
                 x <- parseExpr1
                 let x' = case x of
                              (List [l]) -> l
                              _ -> x
                 return $ List [Atom "quote", x']

parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')')
                     (sepBy parseExpr1 spaces)

symbol :: Parser Char
symbol = oneOf ".!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

instance Show LispVal where
       show = showVal

showVal :: LispVal -> String
showVal lv = case lv of
                 (String s)   -> "\"" ++ s ++ "\""
                 (Atom a)     -> a
                 (Number n)   -> show n
                 (List l)     -> "[" ++ unwordsList l ++ "]"
                 (Bool True)  -> "#t"
                 (Bool False) -> "#f"
                 def@(Def _ _)    -> showDef def

showDef :: LispVal -> String
showDef (Def name body) = "var " ++ name ++ " = " ++ lisp2js body
showDef nonDef = error "called showDef on a non-def value: " ++ show nonDef

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal

data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | ParserErr      ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String

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

hError :: Either LispError String -> Either LispError String
hError = flip catchError (return . show)

catch :: Either LispError a -> a
catch (Right val) = val
catch (Left err) = error . show $ err
