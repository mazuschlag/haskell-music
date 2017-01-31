module Parser.Compiler where

--import Euterpea
import Parser.Tokenizer
import Data.Ratio
import Data.Map (Map, (!))
import Data.Char (toUpper)
import qualified Data.Map as Map

type KeySig = Map.Map Char Char

-- Euterpea Stuff -------------------------------------------------------------
infixr 5 :+:, :=:

type Octave = Int
type Pitch = (PitchClass, Octave)
type Dur = Rational

data PitchClass = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
                | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
                | Bf | Ass | B | Bs | Bss
    deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Primitive a = Note Dur a        
                 | Rest Dur          
    deriving (Show, Eq, Ord)

data Music a  = 
      Prim (Primitive a)               --  primitive value 
    | Music a :+: Music a              --  sequential composition
    | Music a :=: Music a              --  parallel composition
    deriving (Show, Eq, Ord)
--------------------------------------------------------------------------------

-- Compile the notes as Euterpea Music --
compileMusic :: Tokens -> Music Pitch
compileMusic tokens = foldr (:+:) (Prim (Rest 0)) (compilePrims tokens) 

compilePrims :: Tokens -> [Music Pitch]
compilePrims [] = []
compilePrims ((Token f p) : tokens) 
    | f == Tone = toPrim p : compilePrims tokens
    | otherwise = compilePrims tokens

toPrim :: Phrase -> Music Pitch
toPrim ps = Prim (Note (createDur ps) (createPitch ps))

-- Creating a Pitch by getting the note's PitchClass and Octave --
createPitch :: Phrase -> Pitch
createPitch ns = (read (getPitchClass ns), read (getOctave ns))

getPitchClass :: Phrase -> Phrase
getPitchClass [] = []
getPitchClass (n:ns)
    | isSound n     = n : getPitchClass ns
    | isSharpFlat n = convertSharpFlat n : getPitchClass ns 
    | otherwise     = getPitchClass ns

getOctave :: Phrase -> Phrase
getOctave [] = []
getOctave (o:os)
    | isOctave o = o : getOctave os
    | otherwise  = getOctave os
    
-- Interpret the duration of notes --
createDur :: Phrase -> Dur
createDur ds = 
    let durValue = getDur ds
    in foldl calcDur (1/1) durValue 

getDur :: Phrase -> Phrase
getDur [] = []
getDur (d:ds)
    | isDuration d = d : getDur ds
    | otherwise    = getDur ds

calcDur :: Dur -> Char -> Dur
calcDur d '\\' = d * 1/2
calcDur d 'o'  = d
calcDur d '.'  = d + (d * 1/2)
calcDur d ','  = d + ((d * 1/2) + (d * 1/4))
calcDur d '>'  = d * 1/4
calcDur d ')'  = d * 1/8
calcDur d _    = d

-- Get and apply the Key Signature to necessary notes --
applyKeySig :: Tokens -> Tokens
applyKeySig tokens = 
    let keySig = keySigMap tokens
    in mapKeySig tokens keySig
    
mapKeySig :: Tokens -> KeySig -> Tokens
mapKeySig [] _ = []
mapKeySig ((Token f p) : tokens) keySig
    | f == Tone = (Token f (insertKeySig keySig p)) : mapKeySig tokens keySig
    | otherwise = (Token f p) : mapKeySig tokens keySig
    
insertKeySig :: KeySig -> Phrase -> Phrase
insertKeySig keySig [] = []
insertKeySig keySig (p:ps)
    | (isSound p) && (Map.member p keySig) = p : keySig ! p : insertKeySig keySig ps 
    | otherwise                            = p : insertKeySig keySig ps

keySigMap :: Tokens -> KeySig
keySigMap tokens =
    let keySig = getKeySig tokens
        notes = [n | n <- keySig, isSound n]
        sharpsFlats = [sf | sf <- keySig, isSharpFlat sf]
        nSF = toTuples notes sharpsFlats
    in Map.fromList nSF

getKeySig :: Tokens -> String
getKeySig tokens = concat (keySig tokens) 
    where keySig tokens = [p | (Token f p) <- tokens, f == Key]

-- Helper functions --
convertSharpFlat :: Char -> Char
convertSharpFlat sF = if sF == '+' then 's' else 'f'

isSharpFlat :: Char -> Bool
isSharpFlat sf = sf `elem` "+-"

isSound :: Char -> Bool
isSound t = t `elem` "ABCDEFG"

isSilence :: Char -> Bool
isSilence r = r == 'r'

isOctave :: Char -> Bool
isOctave o = o `elem` "0123456789"

isDuration :: Char -> Bool
isDuration d = d `elem` "o>)\\.,"

toTuples :: (Ord a) => [a] -> [b] -> [(a, b)]
toTuples [] _ = []
toTuples _ [] = []
toTuples (x:xs) (y:ys) = (x, y) : toTuples xs ys