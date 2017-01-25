module Parser.Error where

import Parser.Tokenizer

data Category = Tone | Rest | Octave | Accidental | Duration | None
    deriving (Show, Ord, Eq, Read)
    
tokenError :: Tokens -> Tokens
tokenError tokens = foldr checkE [] tokens
    where checkE (Token f p) acc = if f /= Error then acc else (Token f p) : acc

checkNotes :: Tokens -> [Bool]
checkNotes tokens = map (noteError None) (getNotes tokens)

getNotes :: Tokens -> Tokens
getNotes tokens = 
    foldr (\(Token f p) acc -> if f == Note then (Token f p) : acc else acc) [] tokens

noteError :: Category -> Token -> Bool
noteError cat (Token f []) = False
noteError cat (Token f (p:ps))
    | (isTone p) && (cat == None || cat == Duration) = noteError Tone (Token f ps)
    | (isRest p) && (cat == None)                    = noteError Rest (Token f ps)
    | (isOctave p) && (cat == Tone)                  = noteError Octave (Token f ps)
    | (isAccidental p) && (cat == Octave)            = noteError Accidental(Token f ps)
    | (isDuration p) && (cat /= None && cat /= Tone) = noteError Duration (Token f ps)
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