module Main where

import Control.Applicative
import Control.Monad

import System.Environment
import System.IO
import System.Process
import System.Console.GetOpt

import Text.Parsec (parse)
import Control.Monad.Except (throwError)

import Data.Char (toLower)

import qualified LispJs as L
import qualified Utils as U
import qualified Parser as P

data OutputType = JS | PY
                deriving (Show)

data Options = Options  {
    optInput  :: String,
    optOutput :: String,
    optLisp :: String,
    optType :: OutputType
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    optInput  = "in.jsp",
    optOutput = "out.jsp",
    optLisp = "",
    optType = JS}

options :: [OptDescr (Options -> Options)]
options = [Option "i" ["input"]     (ReqArg readInput "FILE")     "input file"
          ,Option "o" ["output"]    (ReqArg readOutput "FILE")    "output file"
          ,Option "l" ["lisp-expr"] (ReqArg readLispExpr "stdin") "input lisp s-exprs"
          ,Option "t" ["type"]      (ReqArg readOutputType "type") "output file type"]

readOutputType :: String -> Options -> Options
readOutputType arg opts = opts {optType = type_}
        where type_ = case toLower <$> arg of
                          "js" -> JS
                          "py" -> PY
                          _ -> error $ "invalid outputType: " ++ arg

readLispExpr :: String -> Options -> Options
readLispExpr arg opts = opts {optLisp = arg}

readInput :: String -> Options -> Options
readInput arg opts = opts {optInput = arg}

readOutput :: String -> Options -> Options
readOutput arg opts = opts {optOutput = arg}

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args
              opts = foldl (flip id) defaultOptions actions
              outType = optType opts
          print $ "opts: " ++ show opts
          print $ "nonOpts: " ++ show nonOpts
          print $ "msgs: " ++ show msgs

          expr <- if null $ optLisp opts
                      then liftM readExpr . readFile $ optInput opts
                      else return . readExpr $ optLisp opts
          let out = optOutput opts
          --Show lisp after parsing
          putStrLn . ("lispVal: " ++) . U.catch . liftM show $ expr
          putStrLn . take 60 $ repeat '#'

          --Convert to outType, write to out, and print
          let translated = liftM (traslateTo outType <$>) expr
          print . ("jsVal: " ++) . show $ translated
          putStrLn . take 60 $ repeat '#'
          js <- L.formatJs . U.catch . liftM (L.toJS <$>) $ translated
          print . ("js: " ++) $ js
          putStrLn . take 60 $ repeat '#'
          writeFile out js
          putStrLn $ out ++ ": " ++ show js

          --Execute js, print its result
          jsOutput <- execJS out
          print $ "jsOutput: " ++ jsOutput
    where
        traslateTo outType = case outType of
                                 JS -> L.translate
                                 PY -> undefined

execJS :: String -> IO String
execJS file = do (_, Just hout, _, _) <- createProcess $ (proc "jsc" [file]) { std_out = CreatePipe }
                 hGetContents hout

readExpr :: String -> Either U.CompilerError [L.LispVal]
readExpr input = case parse P.parseExpr "lisp-js" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
