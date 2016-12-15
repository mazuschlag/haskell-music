module Parser.PhraseWriter where

import Parser.Tokenizer

type EPhrase = String

{-
writePhrase :: Tokens -> EPhrase
writePhrase [] = []
writePhrase ((Token f v) : tokens)
    | f == Note = v ++ " 4 qn :+: " ++ (writePhrase tokens)
    | otherwise =  (chordV v) ++ (writePhrase tokens)
    where chordV v = [v !! 0] ++ " 4 qn :=: " ++ [v !! 1] ++ " 4 qn "

finalPhrase :: EPhrase -> EPhrase
finalPhrase ep = take ((length ep) - 5) ep
-}
