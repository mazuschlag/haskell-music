module Parser.IO where

import Data.Ratio
import Data.List.Split
import System.IO
import System.IO.Error
import System.Environment
import Parser.Tokenizer
import Parser.Error
--import Euterpea as Euterpea

start :: IO()
start = parseFile `catchIOError` handler

parseFile :: IO()
parseFile = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let tokens  = tokenizePhrase contents
    putStrLn $ printTokens fileName tokens    

printTokens :: String -> Tokens -> String
printTokens fileName tokens = fileName ++ ": " ++ 
    (concat . map show $ tokens)

handler :: IOError -> IO()
handler e
    | isDoesNotExistError e = putStrLn "Parse Error: file does not exist"
    | isUserError         e = putStrLn "Parse Error: user error on input"
    | isAlreadyInUseError e = putStrLn "Parse Error: file already in use"
    | otherwise             = ioError e
