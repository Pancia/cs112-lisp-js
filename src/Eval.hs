module Eval where

import Control.Applicative
import Control.Monad.Except
import Data.IORef as IORef
import Data.Maybe

import Utils

type IOThrowsError = ExceptT CompilerError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError a -> IO a
runIOThrows action = runExceptT action >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LokiVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LokiVal -> IOThrowsError LokiVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

updVar :: Env -> String -> (LokiVal -> LokiVal) -> IOThrowsError LokiVal
updVar envRef var f = do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                               (liftIO . (flip modifyIORef f))
                               (lookup var env)
                         getVar envRef var

defVar :: Env -> String -> LokiVal -> IOThrowsError LokiVal
defVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LokiVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bs env = liftM (++ env) (mapM addBinding bs)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

-- EVAL EVAL EVAL EVAL!
eval :: Env -> LokiVal -> IOThrowsError LokiVal
eval _ val@(String{}) = return val
eval _ val@(Number{}) = return val
eval _ val@(Bool{}) = return val
eval _ val@(Keyword{}) = return val

eval _ (List _ [Atom _ "quote", val]) = return val
eval env (List _ [Atom _ "if", pred_, then_, else_]) = do
        result <- eval env pred_
        case result of
            Bool _ False -> eval env else_
            _ -> eval env then_
eval env (List _ [Atom _ "set", Atom _ var, form]) =
     eval env form >>= setVar env var
eval env (List _ (Atom _ f : args))
    | isJust (lookup (f ?> "f") primitives) = mapM (eval env) args >>= liftThrows . apply f
    | otherwise = do fn <- getVar (env ?> "env") f
                     let params = getFnArgs fn
                         body = getFnBody fn
                         closure = getClosure fn
                         evalBody e = liftM last $ mapM (eval e) body
                     if length params /= length args
                         then throwError . NumArgs (length params) $ show <$> args
                         else (liftIO $ bindVars closure $ zip params args) >>= evalBody
eval _ list@(List{}) = return list

eval env (Atom{getAtom=name}) = getVar env name
eval env (Def {getDefName=name
              ,getDefBody=fn@(Fn{})}) = defVar env name fn
eval env (Def {getDefName=name
              ,getDefBody=body}) = eval env body >>= defVar env name

eval _ x = throwError . Default $ "Not Implemented yet: " ++ show x

apply :: String -> [LokiVal] -> ThrowsError LokiVal
apply f args = maybe (throwError $ NotFunction "unrecognized primitive function" f)
                     ($ args)
                     (flip lookup primitives f)

--TODO: Change from binop to numOp, boolOp, using sum/product...?
primitives :: [(String, [LokiVal] -> ThrowsError LokiVal)]
primitives = [("+", numBinop (+))
             ,("-", numBinop (-))
             ,("*", numBinop (*))
             ,("/", numBinop div)
             ,("=", numBoolBinop (==))
             ,("==", strBoolBinop (==))
             ,("++", listOp unpackStr String (++))
             ,("and", boolBoolBinop (&&))
             ,("or", boolBoolBinop (||))
             ,("first", first),("rest", rest)
             ,("cons", cons)]
    where
        numBoolBinop  = boolBinop unpackNum
        strBoolBinop  = boolBinop unpackStr
        boolBoolBinop = boolBinop unpackBool

listOp :: (LokiVal -> ThrowsError a) -> (Meta -> a -> LokiVal) -> (a -> a -> a) -> [LokiVal] -> ThrowsError LokiVal
listOp _ _ _ [] = throwError $ Default "listOp expected non empty args"
listOp unpacker constr op args@(x:_) =
        let m = getMeta x
        in mapM unpacker args >>= return . constr m . foldl1 op

rest :: [LokiVal] -> ThrowsError LokiVal
rest [List m (_ : xs)] = return $ List m xs
rest [badArg]          = throwError . TypeMismatch "List" $ show badArg
rest badArgList        = throwError . NumArgs 1 $ show <$> badArgList

first :: [LokiVal] -> ThrowsError LokiVal
first [List _ (x : _)] = return x
first [badArg]         = throwError . TypeMismatch "List" $ show badArg
first badArgList       = throwError . NumArgs 1 $ show <$> badArgList

cons :: [LokiVal] -> ThrowsError LokiVal
cons [x1, List m []] = return $ List m [x1]
cons [x, List m xs] = return . List m $ x : xs
cons badArgList = throwError . NumArgs 2 $ show <$> badArgList

numBinop :: (Integer -> Integer -> Integer) -> [LokiVal] -> ThrowsError LokiVal
numBinop _op           []  = throwError $ NumArgs 2 []
numBinop _op singleVal@[_] = throwError . NumArgs 2 $ show <$> singleVal
numBinop op params@(x:_)   = let m = getMeta x
                             in mapM unpackNum params >>= return . Number m . foldl1 op

boolBinop :: (LokiVal -> ThrowsError a) -> (a -> a -> Bool) -> [LokiVal] -> ThrowsError LokiVal
boolBinop unpacker op args = if length args /= 2
                             then throwError . NumArgs 2 $ show <$> args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     let m = getMeta $ args !! 0
                                     return . Bool m $ left `op` right

unpackNum :: LokiVal -> ThrowsError Integer
unpackNum (Number _ n) = return n
unpackNum notNum       = throwError . TypeMismatch "Number" $ show notNum

unpackStr :: LokiVal -> ThrowsError String
unpackStr (String _ s) = return s
unpackStr notStr = throwError . TypeMismatch "String" $ show notStr

unpackBool :: LokiVal -> ThrowsError Bool
unpackBool (Bool _ b) = return b
unpackBool notBool = throwError . TypeMismatch "String" $ show notBool
