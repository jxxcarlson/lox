{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module MiniParsec where

-- https://hasura.io/blog/parser-combinators-walkthrough/


newtype MPParser x a = MPParser {
  runParser :: [x] -> ([x], Either ParseError a)
}

data ParseError = ParseError String String
  deriving Show

any :: MPParser a a
any = MPParser $ \input -> case input of
  (x:xs) -> (xs, Right x)
  []     -> ([], Left $ ParseError "any" "input consumed")



eof :: MPParser x ()
eof = MPParser $ \input -> case input of
  -- no input left: the parser succeeds
  []    -> ([], Right ())
  -- leftover data: the parser fails
  (c:_) -> (input, Left $ ParseError "eof" "left over input!")

andThen :: MPParser x a -> (a -> MPParser x b) -> MPParser x b
parserA `andThen` f = MPParser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)

(>>=) :: MPParser x a -> (a -> MPParser x b) -> MPParser x b
(>>=) = andThen

fmap_ :: (a -> b) -> MPParser x a -> MPParser x b
fmap_ f p = MPParser $ \input -> 
       case runParser p input of 
           (input2, Left err) -> (input2, Left err)
           (input2, Right a) -> (input2, Right (f a))

ap :: MPParser x (a -> b) -> MPParser x a -> MPParser x b 
ap pf p = 
  MPParser $ \input -> 
    (case runParser pf input of 
        (input2, Left err) -> (input2, Left err)
        (input2, Right f) -> 
            case runParser p input2 of 
                (input3, Left err) -> (input3, Left err)
                (input3, Right a) -> (input3, Right (f a)))

bind :: MPParser x a -> (a -> MPParser x b) -> MPParser x b
bind parserA f = MPParser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)


instance Functor (MPParser x) where
    -- fmap :: (a -> b) -> MPParser x a -> MPParser x b
    fmap f p = 
        MPParser $ \input ->
            case runParser p input of 
                (input2, Left err) -> (input2, Left err)
                (input2, Right a) -> (input2, Right (f a))


instance Applicative (MPParser x) where 
    -- pure :: a -> MPParser a
    pure a = MPParser $ \input -> (input, Right a)
    -- (<*>) :: MPParser (a -> b) -> MPParser a -> MPParser b 
    (<*>) pf p = ap pf p



instance Monad (MPParser x) where
    (>>=) = bind

-- > runParser (satisfy "starts with x" (\c -> c == 'x')) "xyz"
-- ("yz",Right 'x')
-- > runParser (satisfy "starts with x" (\c -> c == 'x')) "axyz"
-- ("axyz", Left (ParseError "starts with x" "'a'"))

satisfy :: String -> (a -> Bool) -> MPParser a a
satisfy description predicate = try $ do
  c <- MiniParsec.any
  if predicate c
    then return c
    else parseError description "satisfy"

parseError :: String -> String -> MPParser x a 
parseError source description =
    MPParser $ \input -> (input, Left $ ParseError source description)

try :: MPParser x a -> MPParser x a
try p = MPParser $ \state -> case runParser p state of
  (_newState, Left err) -> (state, Left err)
  success               -> success


(<|>) :: Eq x => MPParser x a -> MPParser x a -> MPParser x a
p1 <|> p2 = MPParser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

choice :: Eq x => String -> [MPParser x a] -> MPParser x a
choice description ps = foldr (<|>) noMatch ps
  where noMatch = parseError'

parseError' :: MPParser x a 
parseError' =
    MPParser $ \input -> (input, Left $ ParseError "No match" "choice")

many, many1 :: Eq x => MPParser x a -> MPParser x [a]
many  p = many1 p <|> return []
many1 p = do
  first <- p
  rest  <- many p
  return (first:rest)


sepBy, sepBy1 :: Eq x => MPParser x a -> MPParser x s -> MPParser x [a]
sepBy  p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest  <- many (s >> p)
  return (first:rest)


pSequence :: [MPParser x [a]] -> MPParser x [a]
pSequence (p:[]) = p
pSequence (p:ps) = do
    first <- p
    rest <- pSequence ps
    return (first ++ rest)

--- MORE STUFF

