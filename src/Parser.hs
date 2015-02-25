module Parser where

import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec

import Utils

type Parser a = ParsecT String () Identity a

parseExpr :: Parser [LispVal]
parseExpr = many1 $ try (parseExpr1 <* spaces)

parseExpr1 :: Parser LispVal
parseExpr1 = try parseDef
         <|> try parseClass
         <|> parseBasicExpr

parseBasicExpr :: Parser LispVal
parseBasicExpr = try parseAtom
             <|> try parseString
             <|> try parseNumber
             <|> try parseQuoted
             <|> try parseDotProp
             <|> try parseDotFunc
             <|> try parseFn
             <|> try parseNew
             <|> try parseMap
             <|> try parseList

parseMap :: Parser LispVal
parseMap = between (char '{') (char '}') $ do
    keyVals <- manyTill parseKeyVal (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    return $ Map keys vals

parseKeyVal :: Parser (String, LispVal)
parseKeyVal = do
    key <- ident <* spaces1
    val <- parseExpr1 <* spaces
    return (key, val)

parseDotProp :: Parser LispVal
parseDotProp = between (char '(') (char ')') $ do
    void $ string "."
    propName <- ident <* spaces1
    objName <- parseBasicExpr <* spaces1
    return $ Dot propName objName []

parseDotFunc :: Parser LispVal
parseDotFunc = between (char '(') (char ')') $ do
    void $ string "."
    funcName <- ident <* spaces1
    objName <- parseBasicExpr <* spaces1
    params <- parseExpr
    return $ Dot funcName objName params

parseClass :: Parser LispVal
parseClass = between (char '(') (char ')') $ do
    string "defclass" >> spaces1
    className <- ident <* spaces1
    cnstr <- parseConst <* spaces
    classFns <- many . try $ parseClassFn <* spaces
    classVars <- many . try $ parseVars <* spaces
    return $ DefClass className cnstr classFns classVars

parseClassFn :: Parser LispVal
parseClassFn = between (char '(') (char ')') $ do
    fnName <- ident <* spaces1
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ Classfn fnName params body

parseVars :: Parser LispVal
parseVars = between (char '(') (char ')') $ do
    varName <- ident <* spaces1
    body <- parseExpr1
    return $ Classvar varName body

parseConst :: Parser LispVal
parseConst = between (char '(') (char ')') $ do
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ Const params body

parseNew :: Parser LispVal
parseNew = between (char '(') (char ')') $ do
    string "new" >> spaces1
    className <- ident <* spaces1
    body <- parseExpr
    return $ New className body

parseFn :: Parser LispVal
parseFn = between (char '(') (char ')') $ do
    string "fn" >> spaces1
    params <- parseArgs <* spaces
    body <- parseExpr
    return $ Fn params body

parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

parseDef :: Parser LispVal
parseDef = between (char '(') (char ')') $ do
    string "def" >> spaces1
    name <- ident <* spaces1
    body <- parseExpr1
    return $ Def name body

parseAtom :: Parser LispVal
parseAtom = do atom <- ident
               return $ case atom of
                            "true"  -> Bool True
                            "false" -> Bool False
                            _       -> Atom atom

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
                        (sepBy parseExpr1 spaces1)

-----HELPER PARSERS-----
ident :: Parser String
ident = (:) <$> first <*> many rest
    where first = letter <|> symbol
          rest  = letter <|> digit <|> symbol

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space
