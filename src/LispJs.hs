module LispJs where

import Text.Parsec hiding (spaces)

import Data.Functor.Identity (Identity)
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Def String LispVal
             | Fn [String] [LispVal]
             deriving (Eq, Show)

primitives :: M.Map String String
primitives = M.fromList [("log.", "print")
                        ,("+", "plus")
                        ,("-", "minus")
                        ,("=", "eq")
                        ,("*", "mult")]

type SpecialForm = ([LispVal] -> String)
specialForms :: M.Map String SpecialForm
specialForms = M.fromList [("if", if_)]
    where
        if_ :: SpecialForm
        if_ [cond_, then_, else_] = "(" ++ lisp2js cond_ ++ " ? " ++ lisp2js then_ ++ " : " ++ lisp2js else_ ++ ")"
        if_ [cond_, then_] = if_ [cond_, then_, Atom "null"]
        if_ _ = error "wrong args to if"

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

lookupSpecForm :: String -> Maybe SpecialForm
lookupSpecForm s = M.lookup s specialForms

formatJs :: [String] -> IO String
formatJs js = do helperFns <- readFile "helperFunctions.js"
                 let js' = (++ ";") . L.intercalate ";\n" $ js
                 return $ helperFns ++ js'

lisp2js :: LispVal -> String
lisp2js l = case l of
                a@(Atom _) -> atom2js a
                (Number n) -> show n
                (String s) -> "\"" ++ s ++ "\""
                (Bool x) -> toLower <$> show x
                list@(List _) -> list2js list
                def@(Def _ _) -> def2js def
                fn@(Fn _ _) -> fn2js fn

atom2js :: LispVal -> String
atom2js (Atom a) = if last a == '.'
                       then init a
                       else "jsp_" ++ a
atom2js x = catch . throwError $ TypeMismatch "Def" x

fn2js :: LispVal -> String
fn2js (Fn params body) = "function (" ++ params' ++ ") {\n" ++ showBody body ++ "\n}"
    --TODO: Add indentation param, so its: (" "*4*indent) ++ ...
    where params' = L.intercalate ", " . fmap ("jsp_" ++) $ params
          showBody' b = case b of
                            l@(List _) -> lisp2js l
                            _ -> lisp2js b
          showBody [] = []
          showBody (b:q:bs) = showBody' b ++ ";\n" ++ showBody (q:bs)
          showBody [b] = "return " ++ showBody' b
fn2js x = catch . throwError $ TypeMismatch "Def" x

def2js :: LispVal -> String
def2js (Def name body) = "var " ++ name ++ " = " ++ lisp2js body
def2js x = catch . throwError $ TypeMismatch "Def" x

list2js :: LispVal -> String
list2js l = case l of
                (List [Atom "quote", ql]) -> lisp2js ql
                (List (Atom a:args))
                    | isJust $ lookupSpecForm a -> (fromJust $ lookupSpecForm a) args ++ ";"
                    | otherwise -> lookupFn a ++ "(" ++ L.intercalate ", " (fmap lisp2js args) ++ ")"
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
             <|> try parseFn
             <|> try parseList

parseFn :: Parser LispVal
parseFn = between (char '(') (char ')') $ do
        _ <- string "fn" >> spaces
        params <- between (char '[') (char ']')
                  (manyTill (many1 letter <* skipMany space)
                            (lookAhead (char ']'))) <* spaces
        body <- parseExpr
        return $ Fn params body

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap show

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
