module Parser where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec hiding (spaces)

import Utils

type Parser a = ParsecT String String Identity a

parseExpr :: Parser [LokiVal]
parseExpr = many1 $ try (parseExpr1 <* spaces)

parseExpr1 :: Parser LokiVal
parseExpr1 = do void $ many comment
                tag <- optionMaybe parseAnnotation
                s <- getState
                let meta = fromMaybe (M.singleton "fileType" s) $ M.singleton "fileType" <$> tag
                lispVal <- try parseDef
                       <|> try parseClass
                       <|> parseBasicExpr1
                void $ many comment
                return $ setMeta meta lispVal

parseBasicExpr1 :: Parser LokiVal
parseBasicExpr1 = do void $ many comment
                     meta <- getFileType
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
                     return $ setMeta meta lispVal

getFileType :: Parser Meta
getFileType =  do tag <- optionMaybe parseAnnotation
                  s <- getState
                  return . fromMaybe (M.singleton "fileType" s) $ M.singleton "fileType" <$> tag

parseAnnotation :: Parser String
parseAnnotation = string "#+" >> many1 letter <* spaces

parseMap :: Parser LokiVal
parseMap = between (char '{') (char '}') $ do
    keyVals <- manyTill parseKeyVal (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    s <- getState
    return $ Map (M.singleton "fileType" s) keys vals

parseKeyVal :: Parser (String, LokiVal)
parseKeyVal = do
    key <- ident <* spaces1
    val <- parseExpr1 <* spaces
    return (key, val)

parseDotProp :: Parser LokiVal
parseDotProp = between (char '(') (char ')') $ do
    void $ string "."
    propName <- ident <* spaces1
    objName <- parseBasicExpr1 <* spaces1
    s <- getState
    return $ Dot (M.singleton "fileType" s) propName objName []

parseDotFunc :: Parser LokiVal
parseDotFunc = between (char '(') (char ')') $ do
    void $ string "."
    funcName <- ident <* spaces1
    objName <- parseBasicExpr1 <* spaces1
    params <- parseExpr
    s <- getState
    return $ Dot (M.singleton "fileType" s) funcName objName params

parseClass :: Parser LokiVal
parseClass = between (char '(') (char ')') $ do
    string "defclass" >> spaces1
    className <- ident <* spaces1
    cnstr <- parseConst <* spaces
    classFns <- many . try $ parseClassFn <* spaces
    classVars <- many . try $ parseVars <* spaces
    s <- getState
    return $ DefClass (M.singleton "fileType" s) className cnstr classFns classVars

parseClassFn :: Parser LokiVal
parseClassFn = between (char '(') (char ')') $ do
    fnName <- ident <* spaces1
    args <- parseArgs <* spaces
    body <- parseExpr1
    s <- getState
    return $ Classfn (M.singleton "fileType" s) fnName args body

parseVars :: Parser LokiVal
parseVars = between (char '(') (char ')') $ do
    varName <- ident <* spaces1
    body <- parseExpr1
    s <- getState
    return $ Classvar (M.singleton "fileType" s) varName body

parseConst :: Parser LokiVal
parseConst = between (char '(') (char ')') $ do
    params <- parseArgs <* spaces
    body <- parseExpr1
    s <- getState
    return $ Const (M.singleton "fileType" s) params body

parseNew :: Parser LokiVal
parseNew = between (char '(') (char ')') $ do
    string "new" >> spaces1
    className <- ident <* spaces1
    body <- parseExpr
    s <- getState
    return $ New (M.singleton "fileType" s) className body

parseFn :: Parser LokiVal
parseFn = between (char '(') (char ')') $ do
    string "fn" >> spaces1
    params <- parseArgs <* spaces
    body <- parseExpr
    s <- getState
    return $ Fn (M.singleton "fileType" s) params body

parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

parseDef :: Parser LokiVal
parseDef = between (char '(') (char ')') $ do
    string "def" >> spaces1
    name <- ident <* spaces1
    body <- parseExpr1
    s <- getState
    return $ Def (M.singleton "fileType" s) name body

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
                 s <- getState
                 return $ String (M.singleton "fileType" s) x

parseNumber :: Parser LokiVal
parseNumber = do s <- getState
                 liftM (Number (M.singleton "fileType" s) . read) $ many1 digit

parseQuoted :: Parser LokiVal
parseQuoted = do void $ char '\''
                 toQuote <- parseExpr1
                 let m = getMeta toQuote
                     toQuote' = case toQuote of
                              (List _ [l]) -> l
                              x -> x
                 s <- getState
                 return $ List m [Atom (M.singleton "fileType" s) "quote", toQuote']

parseList :: Parser LokiVal
parseList = do s <- getState
               List (M.singleton "fileType" s) <$> between (char '(') (char ')')
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
comment = string ";" >>= void . return (manyTill anyChar newline)
