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

import qualified LispJs as JS
import qualified LispPy as PY
import qualified Utils as U
import qualified Parser as P
import qualified LispJs as JS

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
    optOutput = "out",
    optLisp = "",
    optType = JS}

options :: [OptDescr (Options -> Options)]
options = [Option "i" ["input"]     (ReqArg readInput "FILE")     "input file"
          ,Option "o" ["output"]    (ReqArg readOutput "FILE")    "output filename"
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
              opts = foldl (flip ($)) defaultOptions actions
              outType = optType opts
              outFile = if '.' `elem` tail (optOutput opts)
                            then optOutput opts
                            else optOutput opts ++ ".jsp." ++ (toLower <$> show outType)
          putStrLn $ ">>opts: " ++ show opts
          putStrLn $ ">>nonOpts: " ++ show nonOpts
          putStrLn $ ">>msgs: " ++ show msgs
          putStrLn . take 60 $ repeat '#'

          lispVals <- if null $ optLisp opts
                          then liftM readExpr . readFile $ optInput opts
                          else return . readExpr $ optLisp opts
          --Show lisp after parsing
          putStrLn . (">>lispVals:\n" ++) . U.catch . liftM show $ lispVals
          putStrLn . take 60 $ repeat '#'
          --Convert to outType, write to out, and print
          src <- case outType of
                     JS -> do let jsVals = U.catch . liftM (JS.translate <$>) $ lispVals
                              putStrLn $ ">>jsVals:\n" ++ show jsVals
                              putStrLn . take 60 $ repeat '#'
                              JS.formatJs $ JS.toJS <$> jsVals
                     PY -> do let pyVals = U.catch . liftM (PY.translate <$>) $ lispVals
                              putStrLn $ ">>pyVals:\n" ++ show pyVals
                              putStrLn . take 60 $ repeat '#'
                              PY.formatPy $ PY.toPY <$> pyVals

          putStrLn $ ">>" ++ outFile ++ ":\n" ++ src
          putStrLn . take 60 $ repeat '#'
          writeFile outFile src

          --Execute src, print its result
          print =<< ("stdout: " ++) <$> execSrc outType outFile

execSrc :: OutputType -> String -> IO String
execSrc outType file = do let exe = case outType of
                                     JS -> "cscript"
                                     PY -> "python"
                          (_, Just hout, _, _) <- createProcess $ (proc exe [file]) { std_out = CreatePipe }
                          hGetContents hout

readExpr :: String -> Either U.CompilerError [JS.LispVal]
readExpr input = case parse P.parseExpr "lisp-js" input of
                     Left err -> throwError $ U.ParserErr err
                     Right val -> return val
