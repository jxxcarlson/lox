module ParserHelpers where

import MiniParsec 

type Parser = MPParser Char

isAlpha' :: Char -> Bool
isAlpha' c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZ_"

isAlphaNum' :: Char -> Bool
isAlphaNum' c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZ_0123456789"


isNonzeroDigit :: Char -> Bool 
isNonzeroDigit c = c `elem` "123456789"

isDigit :: Char -> Bool 
isDigit c = c `elem` "0123456789"

digit  = satisfy "digit" isDigit

nonzeroDigit = (\x -> [x]) <$> satisfy "nonzero digit" isNonzeroDigit 

digits = many digit

decimalPoint =  string "."


quotationMark :: Parser String
quotationMark = string "\""

restOfString :: Parser String
restOfString = many (satisfy "restOfString" (\c -> c /= '\"'))

foo = satisfy "foo" (\c -> c /= ' ')

literalString = quotationMark >> restOfString <*quotationMark

integerDigits = (pSequence [nonzeroDigit, digits]) <* spaces

floatDigits_ = (pSequence [integerDigits, decimalPoint, digits]) <* spaces

doubleDigits = choice "double" [try floatDigits_, integerDigits]

double :: Parser Double
double = (\x -> read x) <$> doubleDigits

identifier = (pSequence [alpha', alphaNums']) <* spaces

alpha' :: Parser String
alpha' = (\x -> (x:[])) <$> satisfy "alpha'" isAlpha'

alphaNum' :: Parser Char
alphaNum' = satisfy "alphaNum'" isAlphaNum'

alphaNums' :: Parser String
alphaNums' = (many alphaNum') <* spaces

isSpace :: Char -> Bool
isSpace c = c == ' '
char c = satisfy [c]     (== c)
string = traverse char
space  = satisfy "space" isSpace
spaces = many space
spaces' = space <* spaces
symbol s = string (s ++ " ") <* spaces
symbol' s = string s <* spaces