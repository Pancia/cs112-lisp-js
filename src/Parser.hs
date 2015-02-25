module Parser where

import qualified Data.Map as M
import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec hiding (spaces)

import Utils

type Parser a = ParsecT String () Identity a

parseExpr :: Parser [LokiVal]
parseExpr = many1 $ try (parseExpr1 <* spaces)

parseExpr1 :: Parser LokiVal
parseExpr1 = do void $ many comment
                lispVal <- try parseDef
                       <|> try parseClass
                       <|> parseBasicExpr
                void $ many comment
                return lispVal

parseBasicExpr :: Parser LokiVal
parseBasicExpr = do void $ many comment
                    lispVal <- try parseAtom
                           <|> try parseString
                           <|> try parseNumber
                           <|> try parseQuoted
                           <|> try parseDotProp
                           <|> try parseDotFunc
                           <|> try parseFn
                           <|> try parseNew
                           <|> try parseMap
                           <|> try parseList
                    void $ many comment
                    return lispVal

parseMap :: Parser LokiVal
parseMap = between (char '{') (char '}') $ do
    keyVals <- manyTill parseKeyVal (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    return $ Map M.empty keys vals

parseKeyVal :: Parser (String, LokiVal)
parseKeyVal = do
    key <- ident <* spaces1
    val <- parseExpr1 <* spaces
    return (key, val)

parseDotProp :: Parser LokiVal
parseDotProp = between (char '(') (char ')') $ do
    void $ string "."
    propName <- ident <* spaces1
    objName <- parseBasicExpr <* spaces1
    return $ Dot M.empty propName objName []

parseDotFunc :: Parser LokiVal
parseDotFunc = between (char '(') (char ')') $ do
    void $ string "."
    funcName <- ident <* spaces1
    objName <- parseBasicExpr <* spaces1
    params <- parseExpr
    return $ Dot M.empty funcName objName params

parseClass :: Parser LokiVal
parseClass = between (char '(') (char ')') $ do
    string "defclass" >> spaces1
    className <- ident <* spaces1
    cnstr <- parseConst <* spaces
    classFns <- many . try $ parseClassFn <* spaces
    classVars <- many . try $ parseVars <* spaces
    return $ DefClass M.empty className cnstr classFns classVars

parseClassFn :: Parser LokiVal
parseClassFn = between (char '(') (char ')') $ do
    fnName <- ident <* spaces1
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ Classfn M.empty fnName params body

parseVars :: Parser LokiVal
parseVars = between (char '(') (char ')') $ do
    varName <- ident <* spaces1
    body <- parseExpr1
    return $ Classvar M.empty varName body

parseConst :: Parser LokiVal
parseConst = between (char '(') (char ')') $ do
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ Const M.empty params body

parseNew :: Parser LokiVal
parseNew = between (char '(') (char ')') $ do
    string "new" >> spaces1
    className <- ident <* spaces1
    body <- parseExpr
    return $ New M.empty className body

parseFn :: Parser LokiVal
parseFn = between (char '(') (char ')') $ do
    string "fn" >> spaces1
    params <- parseArgs <* spaces
    body <- parseExpr
    return $ Fn M.empty params body

parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

parseDef :: Parser LokiVal
parseDef = between (char '(') (char ')') $ do
    string "def" >> spaces1
    name <- ident <* spaces1
    body <- parseExpr1
    return $ Def M.empty name body

parseAtom :: Parser LokiVal
parseAtom = do atom <- ident
               return $ case atom of
                            "true"  -> Bool M.empty True
                            "false" -> Bool M.empty False
                            _       -> Atom M.empty atom

parseString :: Parser LokiVal
parseString = do void $ char '"'
                 x <- many (noneOf "\"")
                 void $ char '"'
                 return $ String M.empty x

parseNumber :: Parser LokiVal
parseNumber = liftM (Number M.empty . read) $ many1 digit

parseQuoted :: Parser LokiVal
parseQuoted = do void $ char '\''
                 toQuote <- parseExpr1
                 let m = getMetaData toQuote
                     toQuote' = case toQuote of
                              (List _ [l]) -> l
                              x -> x
                 return $ List m [Atom M.empty "quote", toQuote']

parseList :: Parser LokiVal
parseList = List M.empty <$> between (char '(') (char ')')
                              (sepBy parseExpr1 spaces1)

-----HELPER PARSERS-----
ident :: Parser String
ident = (:) <$> first <*> many rest
    where first = letter <|> symbol
          rest  = letter <|> digit <|> symbol

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/<=>?@^_~"

space' :: Parser()
space' = void space <|> comment

spaces :: Parser ()
spaces = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'

comment :: Parser ()
comment = string "#" >>= void . return (manyTill anyChar newline)
