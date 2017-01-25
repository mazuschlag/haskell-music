module Parser.Compiler where

--import Euterpea
import Parser.Tokenizer
import Parser.Error
import Data.Ratio
import Data.Map (Map, (!))
import qualified Data.Map as Map

type KeySig = Map.Map Char Char

applyKeySig :: Tokens -> Tokens
applyKeySig tokens = 
    let keySig = keySigMap tokens
    in mapKeySig tokens keySig

mapKeySig :: Tokens -> KeySig -> Tokens
mapKeySig [] _ = []
mapKeySig ((Token f p) : tokens) keySig
    | f == Note = (Token f (insertKeySig keySig p)) : mapKeySig tokens keySig
    | otherwise = (Token f p) : mapKeySig tokens keySig
    
insertKeySig :: KeySig -> Phrase -> Phrase
insertKeySig keySig [] = []
insertKeySig keySig (p:ps)
    | (isNote' p) && (Map.member p keySig) = p : keySig ! p : insertKeySig keySig ps 
    | otherwise                            = p : insertKeySig keySig ps

keySigMap :: Tokens -> KeySig
keySigMap tokens =
    let keySig = getKeySig tokens
        notes = [n | n <- keySig, isNote' n]
        sharpsFlats = [sf | sf <- keySig, isSharpFlat sf]
        nSF = toTuples notes sharpsFlats
    in Map.fromList nSF

getKeySig :: Tokens -> String
getKeySig tokens = concat (keySig tokens) 
    where keySig tokens = [p | (Token f p) <- tokens, f == Key]

-- Some helper functions
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
noteMap = Map.fromList([('o',1%1),('>',1%4),(')',1%8)])

isNote' :: Char -> Bool
isNote' n = n `elem` "ABCDEFGabcdefg"

isSharpFlat :: Char -> Bool
isSharpFlat sf = sf `elem` "+-"

toTuples :: (Ord a) => [a] -> [b] -> [(a, b)]
toTuples [] _ = []
toTuples _ [] = []
toTuples (x:xs) (y:ys) = (x, y) : toTuples xs ys