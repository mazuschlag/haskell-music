module Parser.Tokenizer where

import Data.Ratio
import Data.List.Split
import Data.Char (toLower, isSpace)
import Data.Map (Map, (!))

import qualified Data.Map as Map

type Phrase = String

type Tokens = [Token]

data Token = Token {f :: Form, v :: String}

data Form = Instrument | Key | Tempo | Note | Empty | Error   
    deriving (Show, Ord, Eq, Read, Enum, Bounded)

instance Show Token where
    show (Token f v) = (show f) ++ " " ++ v ++ " "

tokenizePhrase :: Phrase -> Tokens
tokenizePhrase phrase = map makeToken (splitOn " " phrase)

makeToken :: String -> Token
makeToken token 
    | isEmpty (strip token)      = tokenizeEmpty (strip token)
    | isInstrument (strip token) = tokenizeInstrument (strip token)
    | isKey (strip token)        = tokenizeKey (strip token)
    | isTempo (strip token)      = tokenizeTempo (strip token)
    | isNote (strip token)       = tokenizeNote (strip token)
    | otherwise                  = tokenizeError (strip token)

tokenizeEmpty :: String -> Token
tokenizeEmpty empty = Token Empty empty

tokenizeInstrument :: String -> Token
tokenizeInstrument instrument = Token Instrument instrument 

tokenizeKey :: String -> Token
tokenizeKey key = Token Key key

tokenizeTempo :: String -> Token
tokenizeTempo tempo = Token Tempo tempo

tokenizeNote :: String -> Token
tokenizeNote note = Token Note note

tokenizeError :: String -> Token
tokenizeError err = Token Error err

isEmpty :: String -> Bool
isEmpty xs = foldl (\acc x -> if isSpace x then acc else False) True xs

isInstrument :: String -> Bool
isInstrument xs = 
    let instruments = [ "violin", "piano"]
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
    let notes = "ABCDEFGRabcdefgr,.>)oO-+" 
    in foldl (\acc x -> if x `elem` notes then acc else False) True xs
    
-- Some Helper functions
strip :: String -> String
strip xs = foldr (\x acc -> if isSpace x then acc else x : acc) [] xs

calcDur :: Rational -> Char -> Rational
calcDur d '.' = d + d * (1%2)
calcDur d ',' = d + d * (3%4)
calcDur d n
    | d == (0%1) = d + noteMap ! n
    | d == (1%8) = if n == ')' then d * (1%2) else (-1%1)
    | otherwise  = (-1%1)

convertSharpFlat :: String -> String
convertSharpFlat [] = []
convertSharpFlat (x:sF) =
    if x == '+' then 's' : convertSharpFlat sF else 'f' : convertSharpFlat sF

noteMap :: Map.Map Char Rational
noteMap = Map.fromList([('O',1%1),('o',1%2),('>',1%4),(')',1%8)])