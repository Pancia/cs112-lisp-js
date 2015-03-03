module Main where

import Control.Applicative
import Control.Monad

import System.Environment
import System.IO
import System.Process
import System.Exit
import System.Console.GetOpt

import Text.Parsec (runParser)
import Control.Monad.Except (throwError)
import Data.Char (toLower)

import qualified LokiJS as JS
import qualified LokiPY as PY
import Utils (OutputType(JS,PY))
import qualified Utils as U
import qualified Parser as P

data Options = Options {
    optInput  :: String,
    optOutput :: String,
    optLisp :: String,
    optType :: OutputType,
    optRun :: Bool,
    optHelp :: Bool
    } deriving (Show)

fileType :: String
fileType = "loki"

defaultOptions :: Options
defaultOptions = Options {
    optInput  = "in." ++ fileType,
    optOutput = "out",
    optLisp = "",
    optType = JS,
    optRun = True,
    optHelp = False
    }

options :: [OptDescr (Options -> Options)]
options = [Option "i" ["input-file"]  (ReqArg readInput "FILE")      "input file"
          ,Option "o" ["output-file"] (ReqArg readOutput "FILE")     "output filename"
          ,Option "l" ["lisp-expr"]   (ReqArg readLispExpr "stdin")  "input lisp s-exprs"
          ,Option "t" ["type"]        (ReqArg readOutputType "type") "output file type"
          ,Option "c" ["compile"]     (NoArg readCompile)            "should just compile"
          ,Option "h" ["help"]        (NoArg readHelp)               "help with prg usage"]
    where
        readOutputType arg opts = opts {optType = read arg}
        readLispExpr arg opts = opts {optLisp = arg}
        readInput arg opts = opts {optInput = arg}
        readOutput arg opts = opts {optOutput = arg}
        readCompile opts = opts {optRun = False}
        readHelp opts = opts {optHelp = True}

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args
              opts = foldl (flip ($)) defaultOptions actions
              outHelp = optHelp opts
              outType = optType opts
              outRun = optRun opts
              outFile = if '.' `elem` tail (optOutput opts)
                            then optOutput opts
                            else optOutput opts ++ "." ++ fileType ++ "." ++ (toLower <$> show outType)
          putStrLn $ prefix ++ "opts: " ++ show opts
          putStrLn $ prefix ++ "nonOpts: " ++ show nonOpts
          putStrLn $ prefix ++ "msgs: " ++ show msgs
          printDivider

          when outHelp $
              (putStrLn =<< readFile "USAGE") >> exitSuccess

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
                              PY.formatPy $ PY.toPY 0 <$> pyVals
          --Write src to outFile
          writeFile outFile src
          --Print outFile
          printLokiSrc outFile
          printDivider
          --If we should run: switch on outType:
          when outRun $ case outType of
                            --Open test.html in the default browser
                            JS -> void $ openInBrowser "test.html"
                            --Execute src, printing its result
                            PY -> print =<< ("stdout: " ++) <$> execSrc outType outFile
    where
        openInBrowser url = createProcess $ proc (U.caseOS "explorer" "open") [url]
        printAfterHelperFns f = callProcess "sed" ["-n", "-e", "/END LOKI/,$p", f]
        printLokiSrc fileName = U.caseOS (putStrLn =<< readFile fileName)
                                         (printAfterHelperFns fileName)
        prefix = ">>"

execSrc :: OutputType -> String -> IO String
execSrc outType file = do let exe = case outType of
                                     JS -> "node"
                                     PY -> "python"
                          (_, Just hout, _, _) <- createProcess $ (proc exe [file]) { std_out = CreatePipe }
                          hGetContents hout

readExpr :: OutputType -> String -> Either U.CompilerError [U.LokiVal]
readExpr outType input = case runParser P.parseExpr (show outType) "lisp-js" input of
                             Left err -> throwError $ U.ParserErr err
                             Right val -> return val

printDivider :: IO ()
printDivider = putStrLn . take 60 $ repeat '#'
