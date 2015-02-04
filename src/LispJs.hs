module LispJs where

import Text.Parsec hiding (spaces)

import Data.Functor.Identity (Identity)
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Def String LispVal
             | Fn [String] [LispVal]
             deriving (Eq, Show)

primitives :: M.Map String String
primitives = M.fromList [("log", "print")
                        ,("+", "plus")
                        ,("-", "minus")
                        ,("=", "eq")
                        ,("*", "mult")]

type SpecialForm = [JsVal] -> String
specialForms :: M.Map String SpecialForm
specialForms = M.fromList [("if", if_)]
    where
        if_ :: SpecialForm
        if_ [cond_, then_, else_] = "(" ++ toJS cond_ ++ " ? " ++ toJS then_ ++ " : " ++ toJS else_ ++ ")"
        if_ [cond_, then_] = if_ [cond_, then_, JsId "null"]
        if_ _ = error "wrong args to if"

lookupFn :: String -> String
lookupFn f = fromMaybe f $ M.lookup f primitives

lookupSpecForm :: String -> Maybe SpecialForm
lookupSpecForm s = M.lookup s specialForms

formatJs :: [String] -> IO String
formatJs js = do helperFns <- readFile "helperFunctions.js"
                 let js' = (++ ";") . L.intercalate ";\n" $ js
                 return $ helperFns ++ js'

data JsVal = JsVar String JsVal                -- var x = ...
           | JsFn [String] [JsVal]             -- function(...){...}
           | JsStr String                      -- "..."
           | JsBool Bool                       -- true|false
           | JsNum Integer                     -- ..-1,0,1..
           | JsId String                       -- x, foo, ...
           | JsObjCall String [String] [JsVal] -- x.foo.bar(...)
           | JsFnCall String [JsVal]           -- foo(...)
           | JsList [JsVal]                    -- [...]
           | JsThing String                    -- ???
           deriving (Eq, Show)

translate :: LispVal -> JsVal
translate v = case v of
                  (Atom a) -> JsId a
                  (Bool b) -> JsBool b
                  (Def n b) -> JsVar n (translate b)
                  (Fn xs b) -> JsFn xs (translate <$> b)
                  l@(List _) -> list2jsVal l
                  (Number n) -> JsNum n
                  (String s) -> JsStr s
    where
        list2jsVal :: LispVal -> JsVal
        list2jsVal l = case l of
                        (List [Atom "quote", ql]) -> translate ql
                        (List [Atom a]) -> JsFnCall a []
                        (List (Atom a:(arg:args)))
                            | last a == '.' -> JsObjCall a [show $ translate arg] $ translate <$> args
                            | otherwise     -> JsFnCall a $ translate <$> (arg:args)
                        (List xs) -> JsList $ translate <$> xs
                        x -> catch . throwError $ TypeMismatch "List" $ show x

toJS :: JsVal -> String
toJS jv = case jv of
              a@(JsId{})      -> id2js a
              (JsNum n)       -> show n
              (JsStr s)       -> "\"" ++ s ++ "\""
              (JsBool x)      -> toLower <$> show x
              l@(JsList{})    -> list2js l
              v@(JsVar{})     -> var2js v
              f@(JsFn{})      -> fn2js f
              x@(JsObjCall{}) -> objCall2js x
              f@(JsFnCall{})  -> fnCall2js f
              (JsThing x)     -> x

objCall2js :: JsVal -> String
objCall2js (JsObjCall _obj _props _args) = undefined
objCall2js x = catch . throwError . TypeMismatch "JsObjCall" $ show x

fnCall2js :: JsVal -> String
fnCall2js (JsFnCall fn args)
        | isJust specForm = fromJust specForm args
        | otherwise = lookupFn fn ++ "(" ++ args' ++ ")"
    where args' = L.intercalate ", " $ toJS <$> args
          specForm = lookupSpecForm fn
fnCall2js x = catch . throwError . TypeMismatch "JsFnCall" $ show x

id2js :: JsVal -> String
id2js (JsId a) = a
id2js x = catch . throwError . TypeMismatch "JsId" $ show x

fn2js :: JsVal -> String
fn2js (JsFn params body) = "function (" ++ params' ++ ") {\n" ++ showBody body ++ "\n}"
    where params' = L.intercalate ", " params
          showBody [] = []
          showBody (b:q:bs) = toJS b ++ ";\n" ++ showBody (q:bs)
          showBody [b] = "return " ++ toJS b
fn2js x = catch . throwError . TypeMismatch "JsFn" $ show x

var2js :: JsVal -> String
var2js (JsVar name body) = "var " ++ name ++ " = " ++ toJS body
var2js x = catch . throwError . TypeMismatch "JsVar" $ show x

list2js :: JsVal -> String
list2js l = case l of
               (JsList [JsId "quote", ql]) -> toJS ql
               (JsList xs) -> "[" ++ L.intercalate ", " (fmap toJS xs) ++ "]"
               x -> catch . throwError . TypeMismatch "JsList" $ show x

readExpr :: String -> Either CompilerError [LispVal]
readExpr input = case parse parseExpr "lisp-js" input of
                     Left err -> throwError $ ParserErr err
                     Right val -> return val

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

data CompilerError = NumArgs        Integer [String]
                   | TypeMismatch   String String
                   | ParserErr      ParseError
                   | BadSpecialForm String String
                   | NotFunction    String String
                   | UnboundVar     String String
                   | Default        String

instance Show CompilerError where
        show = showError

showError :: CompilerError -> String
showError (UnboundVar msg var)      = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg f)       = msg ++ ": " ++ show f
showError (NumArgs expctd found) =
        "Expected " ++ show expctd ++
        " args; found values " ++ unwords (show <$> found)
showError (TypeMismatch expctd fnd) =
        "Invalid type, expected: " ++ expctd ++
        ", found: " ++ show fnd
showError (ParserErr parseErr) = "ParseErr at " ++ show parseErr
showError (Default err) = err

hError :: Either CompilerError String -> Either CompilerError String
hError = flip catchError (return . show)

catch :: Either CompilerError a -> a
catch (Right val) = val
catch (Left err) = error . show $ err
