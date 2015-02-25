module Parser where

import qualified Data.Map as M
import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec hiding (spaces)

import Utils

type Parser a = ParsecT String () Identity a

parseExpr :: Parser [LispVal]
parseExpr = many1 $ try (parseExpr1 <* spaces)

parseExpr1 :: Parser LispVal
parseExpr1 = do void $ many comment
                lispVal <- try parseDef
                       <|> try parseClass
                       <|> parseBasicExpr
                void $ many comment
                return lispVal

parseBasicExpr :: Parser LispVal
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

parseMap :: Parser LispVal
parseMap = between (char '{') (char '}') $ do
    keyVals <- manyTill parseKeyVal (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    return $ Map M.empty keys vals

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
    return $ Dot M.empty propName objName []

parseDotFunc :: Parser LispVal
parseDotFunc = between (char '(') (char ')') $ do
    void $ string "."
    funcName <- ident <* spaces1
    objName <- parseBasicExpr <* spaces1
    params <- parseExpr
    return $ Dot M.empty funcName objName params

parseClass :: Parser LispVal
parseClass = between (char '(') (char ')') $ do
    string "defclass" >> spaces1
    className <- ident <* spaces1
    cnstr <- parseConst <* spaces
    classFns <- many . try $ parseClassFn <* spaces
    classVars <- many . try $ parseVars <* spaces
    return $ DefClass M.empty className cnstr classFns classVars

parseClassFn :: Parser LispVal
parseClassFn = between (char '(') (char ')') $ do
    fnName <- ident <* spaces1
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ Classfn M.empty fnName params body

parseVars :: Parser LispVal
parseVars = between (char '(') (char ')') $ do
    varName <- ident <* spaces1
    body <- parseExpr1
    return $ Classvar M.empty varName body

parseConst :: Parser LispVal
parseConst = between (char '(') (char ')') $ do
    params <- parseArgs <* spaces
    body <- parseExpr1
    return $ Const M.empty params body

parseNew :: Parser LispVal
parseNew = between (char '(') (char ')') $ do
    string "new" >> spaces1
    className <- ident <* spaces1
    body <- parseExpr
    return $ New M.empty className body

parseFn :: Parser LispVal
parseFn = between (char '(') (char ')') $ do
    string "fn" >> spaces1
    params <- parseArgs <* spaces
    body <- parseExpr
    return $ Fn M.empty params body

parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

parseDef :: Parser LispVal
parseDef = between (char '(') (char ')') $ do
    string "def" >> spaces1
    name <- ident <* spaces1
    body <- parseExpr1
    return $ Def M.empty name body

parseAtom :: Parser LispVal
parseAtom = do atom <- ident
               return $ case atom of
                            "true"  -> Bool M.empty True
                            "false" -> Bool M.empty False
                            _       -> Atom M.empty atom

parseString :: Parser LispVal
parseString = do void $ char '"'
                 x <- many (noneOf "\"")
                 void $ char '"'
                 return $ String M.empty x

parseNumber :: Parser LispVal
parseNumber = liftM (Number M.empty . read) $ many1 digit

parseQuoted :: Parser LispVal
parseQuoted = do void $ char '\''
                 toQuote <- parseExpr1
                 let m = getMetaData toQuote
                     toQuote' = case toQuote of
                              (List _ [l]) -> l
                              x -> x
                 return $ List m [Atom M.empty "quote", toQuote']

parseList :: Parser LispVal
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
