module Parser.Tokenizer where

import Data.Ratio
import Data.List.Split
import Data.Text
--import Data.Char (isSpace, toUpper)
--import Data.Map (Map, (!))
--import qualified Data.Map as Map

type Phrase = String

type Tokens = [Token]

data Token = Token {f :: Form, v :: String, d :: Rational}

data Form = Note | Rest | Chord | Space | Error   
    deriving (Show, Ord, Eq, Read, Enum, Bounded)

instance Show Token where
    show (Token Note v d) = (show Note) ++ " " ++ v ++ " " ++ (show d)
    show (Token Chord v d) = (show Chord) ++ " " ++ v ++ " " ++ (show d)
    show (Token Rest v d) = (show Rest) ++ " " ++ v ++ " " ++ (show d)
    show (Token Error v d) = (show Error) ++ " " ++ v ++ " "
    show (Token Space v d) = v

tokenizePhrase :: Phrase -> Tokens
tokenizePhrase phrase =
    let tN = tokenizeNotes (dropWhile (/= '|') phrase) (Token Space [] (0%1))
        tSF = tokenizeSharpsFlats phrase
    in map (tokenizeFinal tSF) tN

tokenizeFinal :: Map.Map Char Char -> Token -> Token
tokenizeFinal tSF (Token f v d) =
    Token f (concat . map (addSharpFlat tSF) $ v) d
    where addSharpFlat tSF n = if n `Map.member` tSF then [n]++[tSF!n] else [n]

tokenizeSharpsFlats :: Phrase -> Map.Map Char Char
tokenizeSharpsFlats line =
    let notes = filter isNote . takeWhile (/= '|') $ line
        sharpFlat = filter isSharpFlat . takeWhile (/= '|') $ line
    in Map.fromList (zip notes (convertSharpFlat sharpFlat))

-- tokenizeNotes function that relies on whitespace
-- capitalizes all notes and rests
tokenizeNotes :: Phrase -> Token -> Tokens
tokenizeNotes [] (Token f v d) = (Token f v d):[]
tokenizeNotes (n:line) (Token f v d) = case tokenizeForm n f d of
    1 -> appendToken line (Token f v d) (Token Note [n] (0%1))
    2 -> appendToken line (Token f v d) (Token Rest [n] (0%1))
    3 -> tokenizeNotes line (Token f v $ calcDur d n)
    4 -> tokenizeNotes line (Token f (v++(convertSharpFlat[n])) d)
    5 -> appendToken line (Token Chord v d) (Token Chord [n] (0%1))
    6 -> appendToken line (Token f v d) (Token Error [n] (0%1))
    7 -> tokenizeNotes line (Token f (v++[n]) d)
    0 -> appendToken line (Token f v d) (Token Space [n] (0%1))
    where appendToken l oldT newT = oldT : tokenizeNotes l newT

tokenizeForm :: Char -> Form -> Rational -> Int
tokenizeForm n f d
    | (isNote n) && (f == Space || f == Error)     = 1
    | (isRest n) && (f == Space || f == Error)     = 2
    | (isDur n) && (f >= Note && f <= Chord)       = 3
    | (isSharpFlat n) && (f == Note || f == Chord) = 4
    | (isNote n) && (f == Note) && (d > 0%1)       = 5
    | (isSharpFlat n || isDur n) && (f == Space)   = 6
    | (isSharpFlat n || isDur n) && (f == Error)   = 7
    | otherwise                                    = 0

-- Some Helper functions
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

isNote, isDur, isSharpFlat, isRest :: Char -> Bool
isNote n       = n `elem` "abcdefg"
isDur d        = d `elem` "Oo>).,"
isSharpFlat sf = sf `elem` "+-"
isRest r       = r == 'r'
