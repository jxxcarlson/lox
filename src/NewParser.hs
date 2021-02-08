{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module NewParser where

-- https://hasura.io/blog/parser-combinators-walkthrough/


newtype Parser x a = Parser {
  runParser :: [x] -> ([x], Either ParseError a)
}

data ParseError = ParseError String String
  deriving Show

any :: Parser a a
any = Parser $ \input -> case input of
  (x:xs) -> (xs, Right x)
  []     -> ([], Left $ ParseError "any" "input consumed")



eof :: Parser x ()
eof = Parser $ \input -> case input of
  -- no input left: the parser succeeds
  []    -> ([], Right ())
  -- leftover data: the parser fails
  (c:_) -> (input, Left $ ParseError "eof" "left over input!")

andThen :: Parser x a -> (a -> Parser x b) -> Parser x b
parserA `andThen` f = Parser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)

(>>=) :: Parser x a -> (a -> Parser x b) -> Parser x b
(>>=) = andThen

fmap_ :: (a -> b) -> Parser x a -> Parser x b
fmap_ f p = Parser $ \input -> 
       case runParser p input of 
           (input2, Left err) -> (input2, Left err)
           (input2, Right a) -> (input2, Right (f a))

ap :: Parser x (a -> b) -> Parser x a -> Parser x b 
ap pf p = 
  Parser $ \input -> 
    (case runParser pf input of 
        (input2, Left err) -> (input2, Left err)
        (input2, Right f) -> 
            case runParser p input2 of 
                (input3, Left err) -> (input3, Left err)
                (input3, Right a) -> (input3, Right (f a)))

bind :: Parser x a -> (a -> Parser x b) -> Parser x b
bind parserA f = Parser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)


instance Functor (Parser x) where
    -- fmap :: (a -> b) -> Parser x a -> Parser x b
    fmap f p = 
        Parser $ \input ->
            case runParser p input of 
                (input2, Left err) -> (input2, Left err)
                (input2, Right a) -> (input2, Right (f a))


instance Applicative (Parser x) where 
    -- pure :: a -> Parser a
    pure a = Parser $ \input -> (input, Right a)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    (<*>) pf p = ap pf p



instance Monad (Parser x) where
    (>>=) = bind

-- > runParser (satisfy "starts with x" (\c -> c == 'x')) "xyz"
-- ("yz",Right 'x')
-- > runParser (satisfy "starts with x" (\c -> c == 'x')) "axyz"
-- ("axyz", Left (ParseError "starts with x" "'a'"))


satisfy :: String -> (a -> Bool) -> Parser a a
satisfy description predicate = try $ do
  c <- NewParser.any
  if predicate c
    then return c
    else parseError description "satisfy"

parseError :: String -> String -> Parser x a 
parseError source description =
    Parser $ \input -> (input, Left $ ParseError source description)

try :: Parser x a -> Parser x a
try p = Parser $ \state -> case runParser p state of
  (_newState, Left err) -> (state, Left err)
  success               -> success


(<|>) :: Eq x => Parser x a -> Parser x a -> Parser x a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

choice :: Eq x => String -> [Parser x a] -> Parser x a
choice description ps = foldr (<|>) noMatch ps
  where noMatch = parseError'

parseError' :: Parser x a 
parseError' =
    Parser $ \input -> (input, Left $ ParseError "No match" "choice")

many, many1 :: Eq x => Parser x a -> Parser x [a]
many  p = many1 p <|> return []
many1 p = do
  first <- p
  rest  <- many p
  return (first:rest)


sepBy, sepBy1 :: Eq x => Parser x a -> Parser x s -> Parser x [a]
sepBy  p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest  <- many (s >> p)
  return (first:rest)


pSequence :: [Parser x [a]] -> Parser x [a]
pSequence (p:[]) = p
pSequence (p:ps) = do
    first <- p
    rest <- pSequence ps
    return (first ++ rest)

