module Parser.Error where

import Parser.Tokenizer

tokenError :: Tokens -> Tokens
tokenError tokens = foldr checkE [] tokens
    where checkE (Token f v) acc = if f /= Error then acc else (Token f v) : acc