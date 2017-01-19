module Parser.Compiler where

--import Euterpea
import Parser.Tokenizer
import Parser.Error
import Data.Ratio
import Data.Map (Map, (!))
import qualified Data.Map as Map

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