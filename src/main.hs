module Main where

import Control.Applicative
import Control.Monad
import Data.List (isSuffixOf)
import System.Environment
import System.IO
import System.Process
import System.Exit
import System.Console.GetOpt

import Text.Parsec (runParser)
import Control.Monad.Except (throwError)

import qualified LokiJS as JS
import qualified LokiPY as PY
import Utils (OutputType(JS,PY,HTML))
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

defaultOptions :: Options
defaultOptions = Options {
    optInput  = "in.loki",
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
              outFile = if (".loki." ++ show outType) `isSuffixOf` optOutput opts
                            then optOutput opts
                            else optOutput opts ++ ".loki." ++ (show outType)
          putStrLn $ prefix ++ "opts: " ++ show opts
          putStrLn $ prefix ++ "nonOpts: " ++ show nonOpts
          putStrLn $ prefix ++ "msgs: " ++ show msgs
          printDivider

          when outHelp $
              (putStrLn =<< readFile "USAGE") >> exitSuccess

          --If outType is HTML we should parse the input as JS
          let outType' = if outType == HTML then JS else outType
          --If optLisp parse only the passed expression, else use the file
          lispVals <- if null $ optLisp opts
                          then liftM (readExpr outType') . readFile $ optInput opts
                          else return . readExpr outType' $ optLisp opts
          --Show lokiVal
          putStrLn . ((prefix ++ "lispVals:\n") ++) . U.catch . liftM show $ lispVals
          printDivider
          --Convert to outType
          src <- case outType of
                     PY -> do let pyVals = U.catch . liftM (PY.translate <$>) $ lispVals
                              putStrLn $ prefix ++ "pyVals:\n" ++ show pyVals
                              printDivider
                              PY.formatPy $ PY.toPY 0 <$> pyVals
                     -- if out is JS or HTML, translate as JS
                     _ -> do let jsVals = U.catch . liftM (JS.translate <$>) $ lispVals
                             putStrLn $ prefix ++ "jsVals:\n" ++ show jsVals
                             printDivider
                             JS.formatJs $ JS.toJS <$> jsVals
          --If HTML, write js first, then html
          --else write file
          outSrc <- if outType == HTML
                        then do let outFileJs = outFile ++ ".js"
                                writeFile outFileJs src
                                return $ formatHtml outFileJs
                        else return src
          writeFile outFile outSrc
          --Print outFile
          printLokiOutput outFile
          printDivider
          --If we should run: execSrc based on outType
          when outRun $ print =<< ("stdout: " ++) <$> execSrc outType outFile
    where
        printAfterHelperFns f = callProcess "sed" ["-n", "-e", "/END LOKI/,$p", f]
        printLokiOutput fileName = U.caseOS (putStrLn =<< readFile fileName)
                                            (printAfterHelperFns fileName)
        prefix = ">>"

execSrc :: OutputType -> String -> IO String
execSrc outType file = do let exe = case outType of
                                     JS -> "node"
                                     PY -> "python"
                                     HTML -> U.caseOS "explorer" "open"
                          (_, Just hout, _, _) <- createProcess $ (proc exe [file]) { std_out = CreatePipe }
                          hGetContents hout

-- Use to parse a String with a given outType
readExpr :: OutputType -> String -> Either U.CompilerError [U.LokiVal]
readExpr outType input = case runParser P.parseExpr outType' ("lisp-" ++ outType') input of
                             Left err -> throwError $ U.ParserErr err
                             Right val -> return val
    where outType' = show outType

printDivider :: IO ()
printDivider = putStrLn . take 60 $ repeat '#'

formatHtml :: String -> String
formatHtml jsSrcFile = "<!DOCTYPE html>\n"
                    ++ "<html>\n"
                    ++ "<head>\n"
                    ++ "\t<title>Page Title</title>\n"
                    ++ "\t<script src=\"https://code.jquery.com/jquery-1.10.2.js\"></script>\n"
                    ++ "</head>\n"
                    ++ "<body>\n"
                    ++ "\t<script src=\"" ++ jsSrcFile ++ "\"></script>\n"
                    ++ "</body>\n"
                    ++ "</html>\n"
