module Parser where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Applicative hiding ((<|>), many, Const)
import Control.Monad.Identity

import Text.Parsec hiding (spaces)

import Utils

type Parser a = ParsecT String String Identity a

-- Use instead of `parser <?> "expected"`
-- prepends "loki-" to the parser error message
infix 0 >?>
(>?>) :: Parser a -> String -> Parser a
(>?>) x y = x <?> "loki-" ++ y

-- MAIN PARSER
parseExpr :: Parser [LokiVal]
parseExpr = many (parseExpr1 <* spaces)

-- TOP LEVEL PARSERS
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
                                         ,parseNumber
                                         ,parseThing]
                       void $ many comment
                       return (lispVal {getMeta=extra}) >?> "1-literal-expr"

-- Syntactic form that lets you write py|js code
-- without having to write in lisp form,
-- BEWARE: Use sparingly, in conjuction with #+type annotations, and only
-- when the equivalent lisp form isn't available
parseThing :: Parser LokiVal
parseThing = inLitExpr "#{" "}" $ do
    x <- many $ noneOf "}"
    s <- getState
    return $ Thing (newMeta s) x

-- PARSE OO RELATED

-- Parse property accessing `(. propertyName objectName args*)`
-- This however cannot distinguish between calling a zero arg function
--      and a property, but js|py will add extra code to call the property
--      if its a function
parseDot :: Parser LokiVal
parseDot = inLispExpr "." $ do
    funcName <- ident <* spaces1 >?> "dot-func-name"
    objName <- parseBasicExpr1 <* spaces >?> "dot-obj-name"
    args <- liftM (fromMaybe []) (optionMaybe . many $ parseBasicExpr1 <* spaces) >?> "dot-args"
    s <- getState
    return $ Dot (newMeta s) funcName objName args

-- Parse property accessing `(.- propertyName objectName)`
-- Use instead of `.` when you know you want a property and not a zero arg
-- function.
parseProp :: Parser LokiVal
parseProp = inLispExpr ".-" $ do
    propName <- ident <* spaces1 >?> "prop-name"
    objName <- ident <* spaces >?> "prop-obj-name"
    s <- getState
    return $ Prop (newMeta s) propName objName

-- Parse a form to create a new object
parseNew :: Parser LokiVal
parseNew = inLispExpr "new" $ do
    className <- ident <* spaces >?> "class-name"
    body <- many (parseBasicExpr1 <* spaces) >?> "body"
    s <- getState
    return $ New (newMeta s) className body

-- PARSE DEF & FN

-- Parse a function
-- The implementation varies on whether it is contained in a Def or not
--      eg: is either a `def func: ...` or a `(lambda ...)`
parseFn :: Parser LokiVal
parseFn = inLispExpr "fn" $ do
    params <- parseArgs <* spaces >?> "param-list"
    body <- many (parseBasicExpr1 <* spaces) >?> "fn-body"
    s <- getState
    return $ Fn (newMeta s) params body

-- Parse variable assignment
--      while python has no distiction between `var x = ...` and `x = ...`
--      JavaScript does, and as such Def's are limited to only the top
--      level scope
parseDef :: Parser LokiVal
parseDef = inLispExpr "def" $ do
    name <- ident <* spaces >?> "def-name"
    s <- getState
    body <- choice [parseExpr1 <* spaces
                   ,return $ LkiNothing $ newMeta s]
                   >?> "def-body"
    return $ Def (newMeta s) name body

-- PARSE DEFCLASS
--
-- Parse a declaration of a new class
--      It can have super classes, a constructor, instance level
--      functions/properties and static properties.
--      Currently NOT supporting static functions.
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

-- Parse a class function, will be instance level and public
parseClassFn :: Parser LokiVal
parseClassFn = inLispExpr_ $ do
    fnName <- ident <* spaces1 >?> "class-fn-name"
    args <- parseArgs <* spaces >?> "class-fn-params"
    body <- many (parseBasicExpr1 <* spaces) >?> "class-fn-body"
    s <- getState
    return $ Classfn (newMeta s) fnName args body

-- Parse a class property, will be instance level and public
parseVars :: Parser LokiVal
parseVars = inLispExpr_ $ do
    varName <- ident <* spaces1 >?> "class-var-name"
    body <- parseBasicExpr1 <* spaces >?> "class-var-body"
    s <- getState
    return $ Classvar (newMeta s) varName body

-- Parse a constructor:
--Currently limited to parsing `(prop val)`'s inside it, with two exceptions:
--      Calling a super classes constructor should be written as follows:
--          `(super SuperClass args*)
--      Use code inside a `&(...)` to run any non `prop = val` code
--          inside the constructor
parseConst :: Parser LokiVal
parseConst = inLispExpr_ $ do
    params <- parseArgs <* spaces >?> "constr-paramters"
    body <- many ((parseEval <|> parseConstrProp) <* spaces) >?> "constr-body"
    s <- getState
    return $ Constr (newMeta s) params body
    where
        parseEval :: Parser (String, LokiVal)
        parseEval = try (inLitExpr "&(" ")" $ do
                        expr <- many (parseBasicExpr1 <* spaces) >?> "eval-expr"
                        s <- getState
                        return ("eval", List (newMeta s) expr))
        parseConstrProp :: Parser (String, LokiVal)
        parseConstrProp = try (inLispExpr "super" $ do
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
                 digits <- many1 digit
                 decimals <- liftM (fromMaybe "")
                    . optionMaybe $ try ((:) <$> char '.' <*> many1 digit)
                 return $ Number (newMeta s) (digits ++ decimals)

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
                                 ,return . getNumber =<< parseNumber
                                 ,return . pad "\"" . getString =<< parseString]
                                 <* spaces1InLiteral >?> "key")
                         (parseBasicExpr1 <* spacesInLiteral)
    where pad x = (x ++) . (++ x)

---------META HELPERS--------
newMeta :: String -> Meta
newMeta = M.singleton "fileType"

getMetaData :: Parser Meta
getMetaData =  do tag <- optionMaybe $ try parseAnnotation
                  s <- getState
                  return . fromMaybe (newMeta s) $ newMeta <$> tag

parseAnnotation :: Parser String
parseAnnotation = string "#+" >> many1 letter <* spaces

-----GENERAL HELPER PARSERS-----
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
               else return $ encodeID identifier
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
