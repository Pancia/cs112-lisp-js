import Text.Parsec
import Data.Functor.Identity (Identity)
import System.Environment

type Parser a = ParsecT String () Identity a

main :: IO ()
main = print ("hello", "world") >> getArgs >>= print . readExpr . (!! 0)

readExpr :: String -> String
readExpr input = case parse symbol "lisp-js" input of
                     Left err -> "No match: " ++ show err
                     Right val -> "Found: " ++ show val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
