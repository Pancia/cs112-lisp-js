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

-- eg: LokiVals that are valid in the top level
parseExpr1 :: Parser LokiVal
parseExpr1 = do void $ many comment
                extra <- getMetaData
                lispVal <- choice [parseDef
                                  ,parseClass
                                  ,parseBasicExpr1]
                void $ many comment
                return (lispVal {getMeta=extra}) >?> "1-expr"

-- eg: LokiVals that are valid in a Def
parseBasicExpr1 :: Parser LokiVal
parseBasicExpr1 = do void $ many comment
                     extra <- getMetaData
                     lispVal <- choice [parseQuoted
                                       ,try parseProp
                                       ,parseDot
                                       ,parseFn
                                       ,parseNew
                                       ,parseMap
                                       ,parseList
                                       ,parseArray
                                       ,parseLiteralExpr1]
                     void $ many comment
                     return (lispVal {getMeta=extra}) >?> "1-basic-expr"

-- eg: LokiVals that are valid in a Map
parseLiteralExpr1 :: Parser LokiVal
parseLiteralExpr1 = do void $ many comment
                       extra <- getMetaData
                       lispVal <- choice [parseAtom
                                         ,parseTuple
                                         ,parseKeyword
                                         ,parseString
                                         ,parseNumber]
                       void $ many comment
                       return (lispVal {getMeta=extra}) >?> "1-literal-expr"

-- PARSE OO RELATED
parseProp :: Parser LokiVal
parseProp = inLispExpr ".-" $ do
    propName <- ident <* spaces1 >?> "prop-name"
    objName <- ident <* spaces >?> "prop-obj-name"
    s <- getState
    return $ Prop (newMeta s) propName objName

parseDot :: Parser LokiVal
parseDot = inLispExpr "." $ do
    funcName <- ident <* spaces1 >?> "dot-func-name"
    objName <- parseBasicExpr1 <* spaces >?> "dot-obj-name"
    args <- liftM (fromMaybe []) (optionMaybe . many $ parseBasicExpr1 <* spaces) >?> "dot-args"
    s <- getState
    return $ Dot (newMeta s) funcName objName args

parseNew :: Parser LokiVal
parseNew = inLispExpr "new" $ do
    className <- ident <* spaces >?> "class-name"
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
    superClasses <- liftM (fromMaybe []) . optionMaybe
                    $ (inLitExpr "[" "]" . many $ ident <* spaces) <* spaces
    cnstr <- optionMaybe $ try (parseConst <* spaces >?> "class-constructor")
    classFns <- liftM (fromMaybe []) . optionMaybe
                $ (many (try $ parseClassFn <* spaces) >?> "class-functions")
    classVars <- liftM (fromMaybe []) . optionMaybe
                $ (many (try $ parseVars <* spaces) >?> "class-vars")
    s <- getState
    return $ DefClass (newMeta s) className superClasses cnstr classFns classVars

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
    body <- many ((parseEval <|> parseProp) <* spaces) >?> "constr-body"
    s <- getState
    return $ Constr (newMeta s) params body
    where
        parseEval :: Parser (String, LokiVal)
        parseEval = try (inLitExpr "&(" ")" $ do
                        expr <- many (parseBasicExpr1 <* spaces) >?> "eval-expr"
                        s <- getState
                        return ("eval", List (newMeta s) expr))
        parseProp :: Parser (String, LokiVal)
        parseProp = try (inLispExpr "super" $ do
                        superName <- ident <* spaces1 >?> "super-name"
                        args <- many (parseBasicExpr1 <* spaces) >?> "super-args"
                        s <- getState
                        return (superName, ClassSuper (newMeta s) superName args))
                    <|> (inLispExpr_ $ do
                        propName <- ident <* spaces1 >?> "prop-name"
                        propVal <- parseExpr1 <* spaces >?> "prop-val"
                        return (propName, propVal))

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

getMetaData :: Parser Meta
getMetaData =  do tag <- optionMaybe parseAnnotation
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

parseKeyword :: Parser LokiVal
parseKeyword = do void $ char ':'
                  k <- keyword >?> "keyword"
                  s <- getState
                  return $ Keyword (newMeta s) k
    where
        keyword :: Parser String
        keyword = (:) <$> first <*> many rest
        symbol = oneOf ".!$%&*-_+=<>/?"
        first = letter <|> symbol >?> "start-ident"
        rest  = letter <|> digit <|> symbol >?> "rest-ident"

-- PARSE LITERALS: [], {}, ...
parseList :: Parser LokiVal
parseList = do s <- getState
               List (newMeta s)
                    <$> inLispExpr_
                        (sepBy parseExpr1 spaces)

parseArray :: Parser LokiVal
parseArray = do s <- getState
                Array (newMeta s)
                    <$> between (char '[' >> spacesInLiteral)
                                (char ']')
                                (manyTill (parseExpr1 <* spacesInLiteral)
                                          (lookAhead (char ']')))

parseTuple :: Parser LokiVal
parseTuple = inLitExpr "^{" "}" $ do
    vals <- many (parseLiteralExpr1 <* spaces)
    s <- getState
    return $ Tuple (newMeta s) vals

parseMap :: Parser LokiVal
parseMap = between (char '{' >> spacesInLiteral) (char '}') $ do
    keyVals <- manyTill (spacesInLiteral >> parseKeyVal)
                        (lookAhead (char '}'))
    let (keys, vals) = foldl (\(ks, vs) (k, v) -> (k:ks, v:vs)) ([],[]) keyVals
    s <- getState
    return $ Map (newMeta s) keys vals

parseKeyVal :: Parser (String, LokiVal)
parseKeyVal = liftM2 (,) (choice [try ident
                                 ,return . show . getNumber =<< parseNumber
                                 ,return . pad "\"" . getString =<< parseString]
                                 <* spaces1InLiteral >?> "key")
                         (parseBasicExpr1 <* spacesInLiteral)
    where pad x = (x ++) . (++ x)

---------HELPERS--------
newMeta :: String -> Meta
newMeta = M.singleton "fileType"

-----HELPER PARSERS-----
inLispExpr :: String -> Parser a -> Parser a
inLispExpr start = between (try (char '(' >> spaces
                                >> string start <* spaces1)
                                >?> start)
                           (char ')')

inLispExpr_ :: Parser a -> Parser a
inLispExpr_ = between (char '(') (char ')')

inLitExpr :: String -> String -> Parser a -> Parser a
inLitExpr x y = between (string x <* spacesInLiteral)
                        (string y <* spacesInLiteral)

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
