module Main where

import Control.Applicative
import Control.Monad

import System.Environment
import System.IO
import System.Process
import System.Console.GetOpt

import Text.Parsec (runParser)
import Control.Monad.Except (throwError)
import Data.Char (toLower)

import qualified LispJs as JS
import qualified LispPy as PY
import Utils(OutputType(JS,PY))
import qualified Utils as U
import qualified Parser as P

data Options = Options  {
    optInput  :: String,
    optOutput :: String,
    optLisp :: String,
    optType :: OutputType
    } deriving (Show)

fileType :: String
fileType = "loki"

defaultOptions :: Options
defaultOptions = Options {
    optInput  = "in." ++ fileType,
    optOutput = "out",
    optLisp = "",
    optType = JS}

options :: [OptDescr (Options -> Options)]
options = [Option "i" ["input"]     (ReqArg readInput "FILE")     "input file"
          ,Option "o" ["output"]    (ReqArg readOutput "FILE")    "output filename"
          ,Option "l" ["lisp-expr"] (ReqArg readLispExpr "stdin") "input lisp s-exprs"
          ,Option "t" ["type"]      (ReqArg readOutputType "type") "output file type"]

readOutputType :: String -> Options -> Options
readOutputType arg opts = opts {optType = read arg}

readLispExpr :: String -> Options -> Options
readLispExpr arg opts = opts {optLisp = arg}

readInput :: String -> Options -> Options
readInput arg opts = opts {optInput = arg}

readOutput :: String -> Options -> Options
readOutput arg opts = opts {optOutput = arg}

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args
              opts = foldl (flip ($)) defaultOptions actions
              outType = optType opts
              outFile = if '.' `elem` tail (optOutput opts)
                            then optOutput opts
                            else optOutput opts ++ "." ++ fileType ++ "." ++ (toLower <$> show outType)
          putStrLn $ prefix ++ "opts: " ++ show opts
          putStrLn $ prefix ++ "nonOpts: " ++ show nonOpts
          putStrLn $ prefix ++ "msgs: " ++ show msgs
          printDivider

          lispVals <- if null $ optLisp opts
                          then liftM (readExpr outType) . readFile $ optInput opts
                          else return . readExpr outType $ optLisp opts
          --Show lokiVal
          putStrLn . ((prefix ++ "lispVals:\n") ++) . U.catch . liftM show $ lispVals
          printDivider
          --Convert to outType
          src <- case outType of
                     JS -> do let jsVals = U.catch . liftM (JS.translate <$>) $ lispVals
                              putStrLn $ prefix ++ "jsVals:\n" ++ show jsVals
                              printDivider
                              JS.formatJs . filter (/= "") $ JS.toJS <$> jsVals
                     PY -> do let pyVals = U.catch . liftM (PY.translate <$>) $ lispVals
                              putStrLn $ prefix ++ "pyVals:\n" ++ show pyVals
                              printDivider
                              PY.formatPy $ PY.toPY <$> pyVals
          --Print out.loki.*
          putStrLn $ prefix ++ "" ++ outFile ++ ":\n" ++ src
          printDivider
          --Write src to outFile
          writeFile outFile src
          --Switch on outType:
          case outType of
              --Open test.html in the default browser
              JS -> void $ openInBrowser "test.html"
              --Execute src, printing its result
              PY -> print =<< ("stdout: " ++) <$> execSrc outType outFile
    where
        openInBrowser url = createProcess $ proc (U.caseWindowsOrOther "explorer" "open") [url]
        prefix = ">>"

execSrc :: OutputType -> String -> IO String
execSrc outType file = do let exe = case outType of
                                     JS -> U.caseWindowsOrOther "cscript" "jsc"
                                     PY -> "python"
                          (_, Just hout, _, _) <- createProcess $ (proc exe [file]) { std_out = CreatePipe }
                          hGetContents hout

readExpr :: OutputType -> String -> Either U.CompilerError [U.LokiVal]
readExpr outType input = case runParser P.parseExpr (show outType) "lisp-js" input of
                             Left err -> throwError $ U.ParserErr err
                             Right val -> return val

printDivider :: IO ()
printDivider = putStrLn . take 60 $ repeat '#'
