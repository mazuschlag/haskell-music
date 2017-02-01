module Parser.Tokenizer where

import Data.List.Split
import Data.Char (toUpper, toLower, isSpace)

type Phrase = String

type Tokens = [Token]

data Token = Token {f :: Form, p :: Phrase}

data Form = Instrument | Key | Tempo | Tone | Chord | Silent | Empty | Grammar 
            | Error   
    deriving (Show, Ord, Eq, Read, Enum, Bounded)

instance Show Token where
    show (Token Empty p) = show Empty ++ " "
    show (Token f p) = (show f) ++ " " ++ p ++ " "

tokenizePhrase :: Phrase -> Tokens
tokenizePhrase phrase = map makeToken (splitOn " " phrase)

makeToken :: String -> Token
makeToken token 
    | isEmpty sToken      = tokenizeEmpty sToken
    | isInstrument sToken = tokenizeInstrument sToken
    | isKey sToken        = tokenizeKey sToken
    | isTempo sToken      = tokenizeTempo sToken
    | isTone sToken       = tokenizeTone sToken
    | isRest sToken       = tokenizeRest sToken
    | isGrammar sToken    = tokenizeGrammar sToken
    | otherwise           = tokenizeError sToken
    where sToken = strip token
    
tokenizeEmpty :: String -> Token
tokenizeEmpty empty = Token Empty empty

tokenizeInstrument :: String -> Token
tokenizeInstrument instrument = Token Instrument instrument 

tokenizeKey :: String -> Token
tokenizeKey key = Token Key (map toUpper key)

tokenizeTempo :: String -> Token
tokenizeTempo tempo = Token Tempo tempo

tokenizeTone :: String -> Token
tokenizeTone note = if isChord note
                    then Token Chord (map toUpper note)
                    else Token Tone (map toUpper note)

tokenizeRest :: String -> Token
tokenizeRest rest = Token Silent (map toUpper rest)

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

isTone :: String -> Bool
isTone xs = 
    let notes = "ABCDEFGabcdefg,.>)o\\+-0123456789" 
    in foldl (\acc x -> if x `elem` notes then acc else False) True xs

isNote :: Char -> Bool
isNote n = n `elem` "ABCDEFGabcdefg"

isChord :: String -> Bool
isChord xs = 
    let total = foldl (\acc x -> if isNote x then acc + 1 else acc) 0 xs
    in total > 1
    
isRest :: String -> Bool
isRest xs =
    let rests = "Rr,.>)o\\" 
    in foldl (\acc x -> if x `elem` rests then acc else False) True xs

isGrammar :: String -> Bool
isGrammar xs =
    let grammar = "{}"
    in foldl (\acc x -> if x `elem` grammar then acc else False) True xs

-- Some Helper functions
strip :: String -> String
strip xs = foldr (\x acc -> if isSpace x then acc else x : acc) [] xs
