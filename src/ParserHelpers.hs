module ParserHelpers where

import MiniParsec 

type Parser = MPParser Char ParseError

-- PREDICATES

isAlpha' :: Char -> Bool
isAlpha' c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZ_"

isAlphaNum' :: Char -> Bool
isAlphaNum' c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZ_0123456789"

isSpace :: Char -> Bool
isSpace c = c == ' '

isNonzeroDigit :: Char -> Bool 
isNonzeroDigit c = c `elem` "123456789"

isDigit :: Char -> Bool 
isDigit c = c `elem` "0123456789"

digit  = satisfy "digit" isDigit

-- SPACE

space  = satisfy "space" isSpace
spaces = many space
spaces' = space <* spaces

-- SYMBOL

symbol s = string (s ++ " ") <* spaces
symbol' s = string s <* spaces

char c = satisfy [c]     (== c)
string = traverse char

-- NUMBERS

nonzeroDigit = (\x -> [x]) <$> satisfy "nonzero digit" isNonzeroDigit 

digits = many digit

decimalPoint =  string "."

doubleDigits = choice "double" [try floatDigits_, integerDigits]

double :: Parser Double
double = (\x -> read x) <$> doubleDigits

-- STRINGS

quotationMark :: Parser String
quotationMark = string "\""

restOfString :: Parser String
restOfString = many (satisfy "restOfString" (\c -> c /= '\"'))

-- LITERAL STRINGS

literalString = quotationMark >> restOfString <*quotationMark

integerDigits = (mpSequence [nonzeroDigit, digits]) <* spaces

floatDigits_ = (mpSequence [integerDigits, decimalPoint, digits]) <* spaces

-- IDENTIFIER

identifier = (mpSequence [alpha', alphaNums']) <* spaces

alpha' :: Parser String
alpha' = (\x -> (x:[])) <$> satisfy "alpha'" isAlpha'

alphaNum' :: Parser Char
alphaNum' = satisfy "alphaNum'" isAlphaNum'

alphaNums' :: Parser String
alphaNums' = (many alphaNum') <* spaces


