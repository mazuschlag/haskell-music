module Parser.Compiler where

import Euterpea
import Euterpea.Music
import Parser.Tokenizer
import Data.Map (Map, (!))
import Data.Char (toUpper)
import qualified Data.Map as Map

type KeySig = Map.Map Char Char

-- Compiles all instruments by blocks
compileAll :: Tokens -> Music Pitch
compileAll tokens =
    let tokenBlocks = splitInstruments tokens ([Token Empty []]) 0
        musicBlocks = map compileMusic tokenBlocks
    in foldr (:=:) (Prim (Rest 0)) musicBlocks

splitInstruments :: Tokens -> Tokens -> Int -> [Tokens]
splitInstruments [] currT _= []
splitInstruments ((Token f p):tokens) currT count
    | (f == Grammar) && (count == 1) = currT : splitInstruments tokens ([Token Empty []]) 0
    | (f == Grammar) && (count == 0) = splitInstruments tokens currT (count+1)
    | otherwise                      = splitInstruments tokens (currT++[Token f p]) count

-- Compile the notes as Euterpea Music --
compileMusic :: Tokens -> Music Pitch
compileMusic tokens = foldr (:+:) (Prim (Rest 0)) (compilePrims tokens)

compilePrims :: Tokens -> [Music Pitch]
compilePrims [] = []
compilePrims ((Token f p) : tokens)
    | f == Tone || f == Silent = toPrim f p : compilePrims tokens
    | f == Chord               = compileChord p : compilePrims tokens
    | otherwise                = compilePrims tokens

toPrim :: Form -> Phrase -> Music Pitch
toPrim Tone ps   = Prim (Note (createDur ps) (createPitch ps))
toPrim Silent ps = Prim (Rest (createDur ps))

-- Split and compile chords --
compileChord :: Phrase -> Music Pitch
compileChord chord =
    let notes = splitChord chord []
        prims = map (toPrim Tone) notes
    in foldr (:=:) (Prim (Rest 0)) prims

splitChord :: Phrase -> Phrase -> [Phrase]
splitChord [] previous = [previous]
splitChord (p:ps) previous
    | (isSound p) && (length previous > 2) = previous : splitChord ps [p]
    | otherwise                            = splitChord ps (previous++[p])

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
createDur ds = calcDur . getDur $ ds

getDur :: Phrase -> Phrase
getDur [] = []
getDur (d:ds)
    | isDuration d = d : getDur ds
    | otherwise    = getDur ds

calcDur :: Phrase -> Dur
calcDur "o"       = wn
calcDur "\\o"     = hn
calcDur ">"       = qn
calcDur ")"       = en
calcDur "\\)"     = sn
calcDur "\\\\)"   = tn
calcDur "\\\\\\)" = sfn
calcDur _         = 0


-- Get and apply the Key Signature to necessary notes --
applyKeySig :: Tokens -> Tokens
applyKeySig tokens =
    let keySig = keySigMap tokens
    in mapKeySig tokens keySig

mapKeySig :: Tokens -> KeySig -> Tokens
mapKeySig [] _ = []
mapKeySig ((Token f p) : tokens) keySig
    | f == Tone || f == Chord = (Token f (insertKeySig keySig p)) : mapKeySig tokens keySig
    | otherwise               = (Token f p) : mapKeySig tokens keySig

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
