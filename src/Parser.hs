module Parser where

-- https://hasura.io/blog/parser-combinators-walkthrough/

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype Parser a = Parser {
  runParser :: String -> (String, Either ParseError a)
}

data ParseError = ParseError String String
  deriving Show

any :: Parser Char
any = Parser $ \input -> case input of
  (x:xs) -> (xs, Right x)
  []     -> ("", Left $ ParseError
    "any character"        -- expected
    "the end of the input" -- encountered
   )


eof :: Parser ()
eof = Parser $ \input -> case input of
  -- no input left: the parser succeeds
  []    -> ("", Right ())
  -- leftover data: the parser fails
  (c:_) -> (input, Left $ ParseError
    "the end of the input" -- expected
    [c]                    -- encountered
   )

-- andThen :: Parser a -> (a -> Parser b) -> Parser b
-- parserA `andThen` f = Parser $ \input ->
--   case runParser parserA input of
--     (restOfInput, Right a) -> runParser (f a) restOfInput
--     (restOfInput, Left  e) -> (restOfInput, Left e)

--  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
--  (>>=) = andThen

fmap_ :: (a -> b) -> Parser a -> Parser b
fmap_ f p = Parser $ \input -> 
       case runParser p input of 
           (input2, Left err) -> (input2, Left err)
           (input2, Right a) -> (input2, Right (f a))

ap :: Parser (a -> b) -> Parser a -> Parser b 
ap pf p = 
  Parser $ \input -> 
    (case runParser pf input of 
        (input2, Left err) -> (input2, Left err)
        (input2, Right f) -> 
            case runParser p input2 of 
                (input3, Left err) -> (input3, Left err)
                (input3, Right a) -> (input3, Right (f a)))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind parserA f = Parser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)


instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = 
        Parser $ \input ->
            case runParser p input of 
                (input2, Left err) -> (input2, Left err)
                (input2, Right a) -> (input2, Right (f a))


instance Applicative Parser where 
    -- pure :: a -> Parser a
    pure a = Parser $ \input -> (input, Right a)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    (<*>) pf p = ap pf p



instance Monad Parser where
    (>>=) = bind

-- > runParser (satisfy "starts with x" (\c -> c == 'x')) "xyz"
-- ("yz",Right 'x')
-- > runParser (satisfy "starts with x" (\c -> c == 'x')) "axyz"
-- ("axyz", Left (ParseError "starts with x" "'a'"))
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
  c <- Parser.any
  if predicate c
    then return c
    else parseError description c

parseError :: String -> a -> Parser a 
parseError description x =
    Parser $ \input -> (input, Left $ ParseError description input)

try :: Parser a -> Parser a
try p = Parser $ \state -> case runParser p state of
  (_newState, Left err) -> (state, Left err)
  success               -> success


(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

choice :: String -> [Parser a] -> Parser a
choice description ps = foldr (<|>) noMatch ps
  where noMatch = parseError'



parseError' :: Parser a 
parseError' =
    Parser $ \input -> (input, Left $ ParseError "No match" input)

many, many1 :: Parser a -> Parser [a]
many  p = many1 p <|> return []
many1 p = do
  first <- p
  rest  <- many p
  return (first:rest)


sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy  p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest  <- many (s >> p)
  return (first:rest)


pSequence :: [Parser [a]] -> Parser [a]
pSequence (p:[]) = p
pSequence (p:ps) = do
    first <- p
    rest <- pSequence ps
    return (first ++ rest)


    -- endsWith :: Char -> Parser String
    -- endsWith1 c = do
    --   first <- p
    --   rest  <- many (s >> p)
    --   return (first:rest)


  -- MORE  

-- Parser.runParser (Parser.char 'x') "xyz"
-- ("yz",Right 'x')
char c = satisfy [c]     (== c)

space  = satisfy "space" isSpace

digit  = satisfy "digit" isDigit

nonzeroDigit = (\x -> [x]) <$> satisfy "nonzero digit" isNonzeroDigit 

digits = many digit

decimalPoint =  string "."

integerDigits = (pSequence [nonzeroDigit, digits]) <* spaces

floatDigits_ = (pSequence [integerDigits, decimalPoint, digits]) <* spaces

doubleDigits = choice "double" [try floatDigits_, integerDigits]

double :: Parser Double
double = (\x -> read x) <$> doubleDigits


isSpace :: Char -> Bool
isSpace c = c == ' '

isDigit :: Char -> Bool 
isDigit c = c `elem` "0123456789"

isNonzeroDigit :: Char -> Bool 
isNonzeroDigit c = c `elem` "123456789"

-- (*>) :: Parser a -> Parser b -> Parser b
-- (<*) :: Parser a -> Parser b -> Parser a
-- (<$) :: a -> Parser b -> Parser a

-- -- ignore leading spaces
-- spaces *> value
-- Parser.runParser (Parser.char 'x' <* Parser.spaces) "x   "
-- ("",Right 'x')

-- -- ignore trailing spaces
-- value <* spaces

-- -- substitute a value
-- True <$ string “true”


string = traverse char
spaces = many space
symbol s = string s <* spaces

between open close value = open *> value <* close
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
braces   = between (symbol "{") (symbol "}")


-- P.runParser (P.field "firstName") "firstName: Jim"
-- ("",Right "Jim")
field :: String -> Parser String
field fieldName = string (fieldName ++ ":") >> spaces >> line

field' :: String -> Parser String
field' fieldName = string (fieldName ++ ":") >> spaces >> line

word :: Parser String
word = many (satisfy "not blank" (\c -> c /= ' ' && c /= '\n')) <* spaces

line :: Parser String
line = many (satisfy "not eol" (\c -> c /= '\n')) <* spaces




str = "firstName: Jim\nlastName: Carlson\nemail: jxxcarlson@gmail.com\n"