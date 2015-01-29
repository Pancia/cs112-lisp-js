import Data.Char
import Data.List.Split hiding (oneOf)
import qualified Data.Map as M

import System.IO (hFlush, stdout)
import Text.Parsec

import Data.Functor.Identity (Identity)
import Control.Applicative hiding ((<|>), many)

--TODO: 5+^> => put 5 in c[0], get its value and moveRight that many times
--      arg syntax, eg: "#(do:[>&1<-]) 3+(do!nl)",
--          will execute &1 (1st arg) c[p] (eg: 3) times
--      stdlib?
data BFCommand a = GoRight a          -- >
                 | GoLeft a           -- <
                 | Inc a              -- +
                 | Dec a              -- -
                 | Print a            -- .
                 | Read               -- ,
                 | Debug a            -- ?
                 | Goto a             -- @
                 | Loop [BFCommand a] -- [..]
                 | Comment String     -- {..} or ;..\n or eof
                 | Number a           -- [0-9]+
                 | Fn String          -- (clear)
                 | Defn String String -- #(clear:[-])
                 deriving (Show)

instance Functor BFCommand where
        fmap f (GoRight n) = GoRight (f n)
        fmap f (GoLeft n)  = GoLeft (f n)
        fmap f (Inc n)     = Inc (f n)
        fmap f (Dec n)     = Dec (f n)
        fmap f (Number n)  = Number (f n)
        fmap f (Loop x)    = Loop $ (fmap . fmap) f x
        fmap f (Print n)   = Print (f n)
        fmap f (Debug n)   = Debug (f n)
        fmap f (Goto n)    = Goto (f n)
        fmap _ (Comment c) = Comment c
        fmap _ (Fn f)      = Fn f
        fmap _ (Defn n f)  = Defn n f
        fmap _ Read        = Read

type BFCmd = BFCommand Int
type BFSrc = [BFCmd]

data Tape a  = Tape [a] a [a]
type Cells   = Tape Int
type Program = Tape BFCmd

type Defns = M.Map String String
initFnState :: Defns
initFnState = M.fromList [("clear", "[-]"),
                          ("nl",    "10."),
                          ("bang",  "33.")]

testBFInput :: String
testBFInput = "#(hello:72.101.108+..111.58.[-]) {def hello => prints out 'hello'}" ++
              "(hello)(nl) {println 'hello'}" ++
              ">10+< {set c[1] to \n, goto c[0]} " ++
              "+[,.  {enter echo loop} " ++
              ">.<]  {print newline after each echo} "
main :: IO Cells
main = doBF testBFInput

doBF :: String -> IO Cells
doBF = runBF . parseBF

type Parser a = ParsecT String () Identity a
parseBF :: String -> BFSrc
parseBF []    = []
parseBF input =
        either (error . show) mergeWithNumbers
        $ runParser bfTokens () "" (input ++ "\n") --fix for eof
        where
            bfTokens :: Parser BFSrc
            bfTokens = spaces *> many bfToken
            bfToken :: Parser BFCmd
            bfToken = const (Inc 1)     <$> (char '+' <* spaces)
                  <|> const (Dec 1)     <$> (char '-' <* spaces)
                  <|> const (GoLeft 1)  <$> (char '<' <* spaces)
                  <|> const (GoRight 1) <$> (char '>' <* spaces)
                  <|> const (Print 1)   <$> (char '.' <* spaces)
                  <|> const (Debug 1)   <$> (char '?' <* spaces)
                  <|> const (Goto 1)    <$> (char '@' <* spaces)
                  <|> const Read        <$> (char ',' <* spaces)
                  <|> (Number . read)   <$> (many1 digit <* spaces)

                  <|> (\[n, fn] -> Defn n fn)
                        <$> splitOn ":"
                        <$> between (char '#' *> char '(') (char ')' <* spaces)
                            (manyTill (choice [alphaNum, oneOf ":[]<>+-,."]) $ lookAhead (char ')'))
                  <|> Fn      <$> between (char '(') (char ')' <* spaces)
                                  (manyTill alphaNum $ lookAhead (char ')'))
                  <|> Loop    <$> between (char '[' <* spaces) (char ']' <* spaces)
                                  bfTokens

                  <|> Comment <$> between (char '{' <* spaces) (char '}' <* spaces)
                                  (manyTill anyChar $ lookAhead (char '}'))
                  <|> Comment <$> between (char ';') newline
                                  (manyTill anyChar $ lookAhead newline)

runBF :: BFSrc -> IO Cells
runBF = run initFnState emptyTape . bfSource2Tape
        where bfSource2Tape [] = error "empty prg tape"
              bfSource2Tape (b:bs) = Tape [] b bs

mergeWithNumbers :: BFSrc -> BFSrc
mergeWithNumbers [] = []
mergeWithNumbers [c] = [c]
mergeWithNumbers (c:c':cs) =
        case c of
            Number n -> fmap (const n) c' : mergeWithNumbers cs
            Loop l   -> Loop (mergeWithNumbers l) : mergeWithNumbers (c':cs)
            _        -> c : mergeWithNumbers (c':cs)

emptyTape :: Cells
emptyTape = Tape [] 0 zeros
        where zeros = repeat 0

moveRight :: Int -> Tape a -> Tape a
moveRight _ tape@(Tape _ _ []) = tape
moveRight n tape@(Tape ls p (r:rs))
        | n < 1  = tape--error "moveRight: cant move tape non-positive amount"
        | n == 1 = Tape (p:ls) r rs
        | otherwise = moveRight (n-1) $ Tape (p:ls) r rs

moveLeft :: Int -> Tape a -> Tape a
moveLeft _ tape@(Tape [] _ _) = tape
moveLeft n tape@(Tape (l:ls) p rs)
        | n < 1  = tape--error "moveLeft: cant move tape non-positive amount"
        | n == 1 = Tape ls l (p:rs)
        | otherwise = moveLeft (n-1) $ Tape ls l (p:rs)

resetTape :: Tape a -> Tape a
resetTape tape@(Tape [] _ _) = tape
resetTape (Tape (l:ls) p rs) = resetTape $ Tape ls l (p:rs)

--Interpret the command currently focused on the instruction tape
run :: Defns -> Cells -> Program -> IO Cells
run fns cells@(Tape l p r) prg@(Tape _ instr _) =
        case instr of
            (GoRight n)   -> advance fns (moveRight n cells) prg
            (GoLeft n)    -> advance fns (moveLeft n cells) prg
            (Inc n)       -> advance fns (Tape l (p+n) r) prg
            (Dec n)       -> advance fns (Tape l (p-n) r) prg
            (Goto n)      -> if n < 0
                                 then error "cant go before 0"
                                 else advance fns (moveRight (n-1) . resetTape $ cells) prg
            (Debug n)     -> do if n == 1
                                    then putStr $ show p
                                    else putStr $ show n
                                hFlush stdout
                                advance fns cells prg
            (Print n)     -> do if n == 1
                                    then putChar (chr p)
                                    else putChar (chr n)
                                hFlush stdout
                                advance fns cells prg
            Read          -> do c <- getChar
                                advance fns (Tape l (ord c) r) prg
            (Number _)    -> advance fns cells prg
            (Comment _)   -> advance fns cells prg
            (Defn name f) -> advance (M.insert name f fns) cells prg
            (Fn name)     -> let (x:xs) = maybe (error $ "invalid fn:" ++ name) parseBF
                                          $ M.lookup name fns
                             in do cells' <- run fns cells $ Tape [] x xs
                                   advance fns cells' prg
            (Loop [])     -> advance fns cells prg
            (Loop (x:xs)) -> do cells'@(Tape _ p' _) <- run fns cells $ Tape [] x xs
                                if p' == 0
                                    then advance fns cells' prg
                                    else run fns cells' prg

--Move the instruction tape to the next instruction
advance :: Defns -> Cells -> Program -> IO Cells
advance _ cells (Tape _ _ []) = return cells
advance fns cells prg = run fns cells (moveRight 1 prg)
