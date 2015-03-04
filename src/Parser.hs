module Parser where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec hiding (spaces)

import Utils

type Parser a = ParsecT String String Identity a

infix 0 >?>
(>?>) :: Parser a -> String -> Parser a
(>?>) x y = x <?> "loki-" ++ y

-- TOP LEVEL PARSERS
parseExpr :: Parser [LokiVal]
parseExpr = many (parseExpr1 <* spaces)

parseExpr1 :: Parser LokiVal
parseExpr1 = do void $ many comment
                extra <- getFileType
                lispVal <- choice [parseDef
                                  ,parseClass
                                  ,parseBasicExpr1]
                void $ many comment
                return (lispVal {getMeta=extra}) >?> "1-expr"

parseBasicExpr1 :: Parser LokiVal
parseBasicExpr1 = do void $ many comment
                     extra <- getFileType
                     lispVal <- choice [parseAtom
                                       ,parseString
                                       ,parseNumber
                                       ,parseQuoted
                                       ,parseDot
                                       ,parseFn
                                       ,parseNew
                                       ,parseMap
                                       ,parseList
                                       ,parseArray]
                     void $ many comment
                     return (lispVal {getMeta=extra}) >?> "1-basic-expr"

-- PARSE OO RELATED
parseDot :: Parser LokiVal
parseDot = inLispExpr "." $ do
    funcName <- ident <* spaces1 >?> "dot-func-name"
    objName <- parseBasicExpr1 <* spaces >?> "dot-obj-name"
    args <- liftM (fromMaybe []) (optionMaybe . many $ parseBasicExpr1 <* spaces) >?> "dot-args"
    s <- getState
    return $ Dot (newMeta s) funcName objName args

parseNew :: Parser LokiVal
parseNew = inLispExpr "new" $ do
    className <- ident <* spaces1 >?> "class-name"
    body <- many (parseBasicExpr1 <* spaces) >?> "body"
    s <- getState
    return $ New (newMeta s) className body

-- PARSE DEF & FN
parseFn :: Parser LokiVal
parseFn = inLispExpr "fn" $ do
    params <- parseArgs <* spaces >?> "param-list"
    body <- many (parseBasicExpr1 <* spaces) >?> "fn-body"
    s <- getState
    return $ Fn (newMeta s) params body

parseDef :: Parser LokiVal
parseDef = inLispExpr "def" $ do
    name <- ident <* spaces >?> "def-name"
    s <- getState
    body <- choice [parseExpr1 <* spaces
                   ,return $ LkiNothing $ newMeta s]
                   >?> "def-body"
    return $ Def (newMeta s) name body

-- PARSE DEFCLASS
parseClass :: Parser LokiVal
parseClass = inLispExpr "defclass" $ do
    className <- ident <* spaces1 >?> "class-name"
    cnstr <- parseConst <* spaces >?> "class-constructor"
    classFns <- many (try $ parseClassFn <* spaces) >?> "class-functions"
    classVars <- many (try $ parseVars <* spaces) >?> "class-vars"
    s <- getState
    return $ DefClass (newMeta s) className cnstr classFns classVars

parseClassFn :: Parser LokiVal
parseClassFn = inLispExpr_ $ do
    fnName <- ident <* spaces1 >?> "class-fn-name"
    args <- parseArgs <* spaces >?> "class-fn-params"
    body <- parseExpr1 >?> "class-fn-body"
    s <- getState
    return $ Classfn (newMeta s) fnName args body

parseVars :: Parser LokiVal
parseVars = inLispExpr_ $ do
    varName <- ident <* spaces1 >?> "class-var-name"
    body <- parseExpr1 >?> "class-var-body"
    s <- getState
    return $ Classvar (newMeta s) varName body

parseConst :: Parser LokiVal
parseConst = inLispExpr_ $ do
    params <- parseArgs <* spaces >?> "constr-paramters"
    body <- many (parseProp <* spaces) >?> "constr-body"
    s <- getState
    return $ Constr (newMeta s) params body
    where
        parseProp :: Parser (String, LokiVal)
        parseProp = inLispExpr_ $ do
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
parseAtom = do atom <- ident >?> "atom"
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
               List (newMeta s)
                    <$> inLispExpr_
                        (sepBy parseExpr1 spacesInLiteral)

parseArray :: Parser LokiVal
parseArray = do s <- getState
                List (newMeta s)
                    <$> between (char '[') (char ']')
                        (sepBy parseExpr1 spacesInLiteral)

parseMap :: Parser LokiVal
parseMap = between (char '{') (char '}') $ do
    keyVals <- manyTill parseKeyVal (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    s <- getState
    return $ Map (newMeta s) keys vals

parseKeyVal :: Parser (String, LokiVal)
parseKeyVal = liftM2 (,) (choice [try ident,
                                 return . pad "\"". getString =<< parseString]
                                 <* spaces1InLiteral >?> "key")
                         (parseBasicExpr1 <* spacesInLiteral)
    where pad x = (x ++) . (++ x)

---------HELPERS--------
newMeta :: String -> Meta
newMeta = M.singleton "fileType"

-----HELPER PARSERS-----
inLispExpr :: String -> Parser LokiVal -> Parser LokiVal
inLispExpr start = between (try (char '(' >> spaces
                                >> string start <* spaces1)
                                >?> start)
                           (char ')')

inLispExpr_ :: Parser a -> Parser a
inLispExpr_ = between (char '(') (char ')')

parseArgs :: Parser [String]
parseArgs = between (char '[') (char ']')
                    (manyTill (ident <* spaces)
                              (lookAhead (char ']')))

reserved :: [String]
reserved = ["def", "defclass", "fn", "new"]

ident :: Parser String
ident = do identifier <- (:) <$> first <*> many rest
           if identifier `elem` reserved
               then unexpected $ "reserved word " ++ show identifier
               else return identifier
    where symbol = oneOf "!$%&*-_+=<>/?"
          first = letter <|> symbol >?> "start-ident"
          rest  = letter <|> digit <|> symbol >?> "rest-ident"

space' :: Parser ()
space' = void space <|> comment

spaces :: Parser ()
spaces = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'

spacesInLiteral :: Parser ()
spacesInLiteral = skipMany (choice [space'
                                   ,void (oneOf ":,")])
                  >?> "spaces-in-literal"

spaces1InLiteral :: Parser ()
spaces1InLiteral = skipMany1 (choice [space'
                                     ,void (oneOf ":,")])
                   >?> "1+spaces-in-literal"

comment :: Parser ()
comment = string ";"
          >>= return (manyTill anyChar newline)
          >>  return ()
          >?> "comment"
