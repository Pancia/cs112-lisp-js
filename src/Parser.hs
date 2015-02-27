module Parser where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec hiding (spaces)

import Utils

type Parser a = ParsecT String String Identity a

-- HIGH LEVEL PARSERS
parseExpr :: Parser [LokiVal]
parseExpr = many1 $ try (parseExpr1 <* spaces)

parseExpr1 :: Parser LokiVal
parseExpr1 = do void $ many comment
                meta <- getFileType
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
                            <|> try parseArray
                     void $ many comment
                     return $ setMeta meta lispVal

-- PARSE OBJECT RELATED
parseDotProp :: Parser LokiVal
parseDotProp = between (char '(') (char ')') $ do
    void $ string "."
    propName <- ident <* spaces1
    objName <- parseBasicExpr1 <* spaces
    s <- getState
    return $ Dot (newMeta s) propName objName []

parseDotFunc :: Parser LokiVal
parseDotFunc = between (char '(') (char ')') $ do
    void $ string "."
    funcName <- ident <* spaces1
    objName <- parseBasicExpr1 <* spaces
    params <- parseExpr
    s <- getState
    return $ Dot (newMeta s) funcName objName params

parseNew :: Parser LokiVal
parseNew = between (char '(') (char ')') $ do
    string "new" >> spaces1
    className <- ident <* spaces1
    body <- parseExpr
    s <- getState
    return $ New (newMeta s) className body

-- PARSE DEF & FN
parseFn :: Parser LokiVal
parseFn = between (char '(') (char ')') $ do
    string "fn" >> spaces1
    params <- parseArgs <* spaces
    body <- parseExpr
    s <- getState
    return $ Fn (newMeta s) params body

parseDef :: Parser LokiVal
parseDef = between (char '(') (char ')') $ do
    string "def" >> spaces1
    name <- ident
    s <- getState
    body <- (spaces1 >> parseExpr1) <|> return (PleaseIgnore $ newMeta s)
    return $ Def (newMeta s) name body

-- PARSE DEFCLASS
parseClass :: Parser LokiVal
parseClass = between (char '(') (char ')') $ do
    string "defclass" >> spaces1
    className <- ident <* spaces1
    cnstr <- parseConst <* spaces
    classFns <- many . try $ parseClassFn <* spaces
    classVars <- many . try $ parseVars <* spaces
    s <- getState
    return $ DefClass (newMeta s) className cnstr classFns classVars

parseClassFn :: Parser LokiVal
parseClassFn = between (char '(') (char ')') $ do
    fnName <- ident <* spaces1
    args <- parseArgs <* spaces
    body <- parseExpr1
    s <- getState
    return $ Classfn (newMeta s) fnName args body

parseVars :: Parser LokiVal
parseVars = between (char '(') (char ')') $ do
    varName <- ident <* spaces1
    body <- parseExpr1
    s <- getState
    return $ Classvar (newMeta s) varName body

parseConst :: Parser LokiVal
parseConst = between (char '(') (char ')') $ do
    params <- parseArgs <* spaces
    body <- many (parseProp <* spaces)
    s <- getState
    return $ Const (newMeta s) params body
    where
        parseProp :: Parser (String, LokiVal)
        parseProp = between (char '(') (char ')') $ do
                      propName <- ident <* spaces1
                      propVal <- parseExpr1
                      return (propName, propVal)

-- PARSE SPECIAL FORMS
parseQuoted :: Parser LokiVal
parseQuoted = do void $ char '\''
                 toQuote <- parseExpr1
                 let m = getMeta toQuote
                     toQuote' = case toQuote of
                              (List _ [l]) -> l
                              x -> x
                 s <- getState
                 return $ List m [Atom (newMeta s) "quote", toQuote']

getFileType :: Parser Meta
getFileType =  do tag <- optionMaybe parseAnnotation
                  s <- getState
                  return . fromMaybe (newMeta s) $ newMeta <$> tag

parseAnnotation :: Parser String
parseAnnotation = string "#+" >> many1 letter <* spaces

-- PARSE PRIMITIVES
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
                 return $ String (newMeta s) x

parseNumber :: Parser LokiVal
parseNumber = do s <- getState
                 liftM (Number (newMeta s) . read) $ many1 digit

-- PARSE LITERALS: [], {}, ...
parseList :: Parser LokiVal
parseList = do s <- getState
               List (newMeta s) <$> between (char '(') (char ')')
                               (sepBy parseExpr1 spacesInLiteral)

parseArray :: Parser LokiVal
parseArray = do s <- getState
                List (newMeta s) <$> between (char '[') (char ']')
                                (sepBy parseExpr1 spacesInLiteral)

parseMap :: Parser LokiVal
parseMap = between (char '{') (char '}') $ do
    keyVals <- manyTill parseKeyVal (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    s <- getState
    return $ Map (newMeta s) keys vals

parseKeyVal :: Parser (String, LokiVal)
parseKeyVal = do
    key <- ident <* spaces1InLiteral
    val <- parseExpr1 <* spacesInLiteral
    return (key, val)

---------HELPERS--------
newMeta :: String -> Meta
newMeta = M.singleton "fileType"

-----HELPER PARSERS-----
parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

ident :: Parser String
ident = (:) <$> first <*> many rest
    where first = letter <|> symbol
          rest  = letter <|> digit <|> symbol

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/<=>?@^_~"

space' :: Parser ()
space' = void space <|> comment

spaces :: Parser ()
spaces = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'

spacesInLiteral :: Parser ()
spacesInLiteral = skipMany $ space' <|> comment <|> void (oneOf ",")

spaces1InLiteral :: Parser ()
spaces1InLiteral = skipMany1 $ space' <|> comment <|> void (oneOf ",")

comment :: Parser ()
comment = string ";" >>= void . return (manyTill anyChar newline)
