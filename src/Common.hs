module Common where

import Text.ParserCombinators.Parsec

type Parser a = (String -> Either ParseError a)
