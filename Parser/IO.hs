module Parser.IO where

import Data.List.Split
import System.IO
import System.IO.Error
import System.Environment
import Parser.Tokenizer
import Parser.Error
import Parser.Compiler
import Euterpea

start :: IO()
start = parseFile `catchIOError` handler

parseFile :: IO()
parseFile = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    --tErrors = tokenError tokens
    --putStrLn $ printTokens fileName (applyKeySig tokens)
    --putStrLn $ show (checkNotes tokens)
    --putStrLn $ printTokenError fileName tErrors
    --putStrLn $ show . checkPitchClass $ tokens
    --putStrLn $ show . checkOctave $ tokens
    --putStrLn $ show . checkDur $ tokens
    --putStrLn $ show . checkPitch $ tokens
    --putStrLn $ show . compileAll . applyKeySig . tokenizePhrase $ contents
    play . compileAll . applyKeySig . tokenizePhrase $ contents

printTokens :: String -> Tokens -> String
printTokens fileName tokens = fileName ++ ":\n\t" ++
    (concat . map show $ tokens)

printTokenError :: String -> Tokens -> String
printTokenError fileName tokens = "Token Error:" ++ fileName ++ ":\n\t" ++
    (concat . map show $ tokens)

handler :: IOError -> IO()
handler e
    | isDoesNotExistError e = putStrLn "Parse Error: file does not exist"
    | isUserError         e = putStrLn "Parse Error: user error on input"
    | isAlreadyInUseError e = putStrLn "Parse Error: file already in use"
    | otherwise             = ioError e
