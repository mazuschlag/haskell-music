module Parser.Tokenizer where

import Data.List.Split
import Data.Char (toLower, isSpace)

type Phrase = String

type Tokens = [Token]

data Token = Token {f :: Form, v :: String}

data Form = Instrument | Key | Tempo | Note | Empty | Grammar | Error   
    deriving (Show, Ord, Eq, Read, Enum, Bounded)

instance Show Token where
    show (Token Empty v) = show Empty ++ " "
    show (Token f v) = (show f) ++ " " ++ v ++ " "

tokenizePhrase :: Phrase -> Tokens
tokenizePhrase phrase = map makeToken (splitOn " " phrase)

makeToken :: String -> Token
makeToken token 
    | isEmpty sToken      = tokenizeEmpty sToken
    | isInstrument sToken = tokenizeInstrument sToken
    | isKey sToken        = tokenizeKey sToken
    | isTempo sToken      = tokenizeTempo sToken
    | isNote sToken       = tokenizeNote sToken
    | isGrammar sToken    = tokenizeGrammar sToken
    | otherwise           = tokenizeError sToken
    where sToken = strip token
    
tokenizeEmpty :: String -> Token
tokenizeEmpty empty = Token Empty empty

tokenizeInstrument :: String -> Token
tokenizeInstrument instrument = Token Instrument instrument 

tokenizeKey :: String -> Token
tokenizeKey key = Token Key key

tokenizeTempo :: String -> Token
tokenizeTempo tempo = Token Tempo tempo

tokenizeNote :: String -> Token
tokenizeNote note = Token Note (map toLower note)

tokenizeGrammar :: String -> Token
tokenizeGrammar grammar = Token Grammar grammar

tokenizeError :: String -> Token
tokenizeError err = Token Error err

isEmpty :: String -> Bool
isEmpty xs = foldl (\acc x -> if isSpace x then acc else False) True xs

isInstrument :: String -> Bool
isInstrument xs = 
    let instruments = ["violin", "piano"]
    in (map toLower xs) `elem` instruments

isKey :: String -> Bool
isKey xs = 
    let keys = "ABCDEFGabcdefg+-"
    in foldl (\acc x -> if x `elem` keys then acc else False) True xs

isTempo :: String -> Bool
isTempo xs = 
    let tempos = "1234567890"
    in foldl (\acc x -> if x `elem` tempos then acc else False) True xs

isNote :: String -> Bool
isNote xs = 
    let notes = "ABCDEFGRabcdefgr,.>)o+-0123456789\\" 
    in foldl (\acc x -> if x `elem` notes then acc else False) True xs

isGrammar :: String -> Bool
isGrammar xs =
    let grammar = "{}"
    in foldl (\acc x -> if x `elem` grammar then acc else False) True xs

-- Some Helper functions
strip :: String -> String
strip xs = foldr (\x acc -> if isSpace x then acc else x : acc) [] xs
