module Parser where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity

import Text.Parsec

import qualified LispJs as L

type Parser a = ParsecT String () Identity a

parseExpr :: Parser [L.LispVal]
parseExpr = many1 $ try (parseExpr1 <* spaces)

parseExpr1 :: Parser L.LispVal
parseExpr1 = parseAtom
     <|> try parseString
     <|> try parseNumber
     <|> try parseQuoted
     <|> try parseDotProp
     <|> try parseDotFunc
     <|> try parseDef
     <|> try parseFn
     <|> try parseNew
     <|> try parseClass
     <|> try parseList

parseDotProp :: Parser L.LispVal
parseDotProp = between (char '(') (char ')') $ do
    void $ string "."
    propName <- ident <* spaces1
    objName <- ident <* spaces
    return $ L.Dot propName objName []

parseDotFunc :: Parser L.LispVal
parseDotFunc = between (char '(') (char ')') $ do
    void $ string "."
    funcName <- ident <* spaces1
    objName <- ident <* spaces1
    params <- parseExpr
    return $ L.Dot funcName objName params

parseClass :: Parser L.LispVal
parseClass = between (char '(') (char ')') $ do
    string "defclass" >> spaces1
    className <- ident <* spaces1
    cnstr <- parseConst <* spaces
    classFns <- many . try $ parseClassFn <* spaces
    classVars <- many . try $ parseVars <* spaces
    return $ L.DefClass className cnstr classFns classVars

parseClassFn :: Parser L.LispVal
parseClassFn = between (char '(') (char ')') $ do
    fnName <- ident <* spaces1
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ L.Classfn fnName params body

parseVars :: Parser L.LispVal
parseVars = between (char '(') (char ')') $ do
    varName <- ident <* spaces1
    body <- parseExpr1
    return $ L.Classvar varName body

parseConst :: Parser L.LispVal
parseConst = between (char '(') (char ')') $ do
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ L.Const params body

parseNew :: Parser L.LispVal
parseNew = between (char '(') (char ')') $  do
    string "new" >> spaces1
    className <- ident <* spaces1
    body <- parseExpr
    return $ L.New className body

parseFn :: Parser L.LispVal
parseFn = between (char '(') (char ')') $ do
    string "fn" >> spaces1
    params <- parseArgs <* spaces
    body <- parseExpr
    return $ L.Fn params body

parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

parseDef :: Parser L.LispVal
parseDef = between (char '(') (char ')') $ do
    string "def" >> spaces1
    name <- ident <* spaces1
    body <- parseExpr1
    return $ L.Def name body

parseAtom :: Parser L.LispVal
parseAtom = do atom <- ident
               return $ case atom of
                            "true"  -> L.Bool True
                            "false" -> L.Bool False
                            _       -> L.Atom atom

parseString :: Parser L.LispVal
parseString = do void $ char '"'
                 x <- many (noneOf "\"")
                 void $ char '"'
                 return $ L.String x

parseNumber :: Parser L.LispVal
parseNumber = liftM (L.Number . read) $ many1 digit

parseQuoted :: Parser L.LispVal
parseQuoted = do void $ char '\''
                 x <- parseExpr1
                 let x' = case x of
                              (L.List [l]) -> l
                              _ -> x
                 return $ L.List [L.Atom "quote", x']

parseList :: Parser L.LispVal
parseList = L.List <$> between (char '(') (char ')')
                          (sepBy parseExpr1 spaces1)

-----HELPER PARSERS-----
ident :: Parser String
ident = (:) <$> first <*> many rest
    where first = letter <|> symbol
          rest  = letter <|> digit <|> symbol

symbol :: Parser Char
symbol = oneOf ".!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space
