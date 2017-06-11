module Parser.Error where

import Euterpea
import Parser.Tokenizer
import Parser.Compiler

data Category = Sound | Silence | Octave | Accidental | Duration | None
    deriving (Show, Ord, Eq, Read)

tokenError :: Tokens -> Tokens
tokenError tokens = foldr checkE [] tokens
    where checkE (Token f p) acc = if f /= Error then acc else (Token f p) : acc

checkTones :: Tokens -> [Bool]
checkTones tokens = map (toneError None) (getTones tokens)

getTones :: Tokens -> Tokens
getTones tokens =
    foldr (\(Token f p) acc -> if f == Tone then (Token f p) : acc else acc) [] tokens

toneError :: Category -> Token -> Bool
toneError cat (Token f []) = False
toneError cat (Token f (p:ps))
    | (isSound p) && (cat == None || cat == Duration) = toneError Sound (Token f ps)
    | (isSilence p) && (cat == None)                  = toneError Silence (Token f ps)
    | (isOctave p) && (cat == Sound)                  = toneError Octave (Token f ps)
    | (isSharpFlat p) && (cat == Octave)              = toneError Accidental (Token f ps)
    | (isDuration p) && (cat /= None && cat /= Sound && cat /= Silence) = toneError Duration (Token f ps)
    | otherwise = True

-- Check steps of Compilation --
checkPitch :: Tokens -> [Pitch]
checkPitch [] = []
checkPitch ((Token f p) : tokens)
    | f == Tone = (read (getPitchClass p), read (getOctave p)) : checkPitch tokens
    | otherwise = checkPitch tokens

checkDur :: Tokens -> [Dur]
checkDur [] = []
checkDur ((Token f p) : tokens)
    | f == Tone = createDur p : checkDur tokens
    | otherwise = checkDur tokens

checkPitchClass :: Tokens -> [PitchClass]
checkPitchClass [] = []
checkPitchClass ((Token f p) : tokens)
    | f == Tone = read (getPitchClass p) : checkPitchClass tokens
    | otherwise = checkPitchClass tokens

checkOctave :: Tokens -> [Octave]
checkOctave [] = []
checkOctave ((Token f p) : tokens)
    | f == Tone = read (getOctave p) : checkOctave tokens
    | otherwise = checkOctave tokens
