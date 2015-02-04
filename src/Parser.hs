module Parser where

import Text.Parsec as P hiding (spaces)
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity

import LispJs

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
        void $ string "fn" >> spaces
        params <- between (char '[') (char ']')
                  (manyTill (many1 letter <* skipMany space)
                            (lookAhead (char ']'))) <* spaces
        body <- parseExpr
        return $ Fn params body

parseDef :: Parser LispVal
parseDef = between (char '(') (char ')') $ do
        void $ string "def"
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
parseString = do void $ char '"'
                 x <- many (noneOf "\"")
                 void $ char '"'
                 return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseQuoted :: Parser LispVal
parseQuoted = do void $ char '\''
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
