module Parser.Error where

import Parser.Tokenizer

data Category = Tone | Rest | Octave | Accidental | Duration | None
    deriving (Show, Ord, Eq, Read)
    
tokenError :: Tokens -> Tokens
tokenError tokens = foldr checkE [] tokens
    where checkE (Token f v) acc = if f /= Error then acc else (Token f v) : acc

checkNotes :: Tokens -> [Bool]
checkNotes tokens = map (noteError None) (getNotes tokens)

getNotes :: Tokens -> Tokens
getNotes tokens = 
    foldr (\(Token f v) acc -> if f == Note then (Token f v) : acc else acc) [] tokens

noteError :: Category -> Token -> Bool
noteError cat (Token f []) = False
noteError cat (Token f (v:vs))
    | (isTone v) && (cat == None || cat == Duration) = noteError Tone (Token f vs)
    | (isRest v) && (cat == None)                    = noteError Rest (Token f vs)
    | (isOctave v) && (cat == Tone)                  = noteError Octave (Token f vs)
    | (isAccidental v) && (cat == Octave)            = noteError Accidental(Token f vs)
    | (isDuration v) && (cat /= None && cat /= Tone) = noteError Duration (Token f vs)
    | otherwise = True

--Helper functions
isTone :: Char -> Bool
isTone t = t `elem` "abcdefg"

isRest :: Char -> Bool
isRest r = r == 'r'

isOctave :: Char -> Bool
isOctave o = o `elem` "0123456789"

isAccidental :: Char -> Bool
isAccidental a = a `elem` "+-"

isDuration :: Char -> Bool
isDuration d = d `elem` "o>)\\.,"