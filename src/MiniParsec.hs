{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module MiniParsec where

{-
MiniParsec is a built-from=scratch, simplified version of the
Parsec combinator library which is basd on the blog post
https://hasura.io/blog/parser-combinators-walkthrough/
-}

-- TYPES

{-
Parameters:
   s -- for stream (of Char, Token, etc.)
   e -- the error type
   a -- what running the parser yields
-}
newtype MPParser s e a = MPParser {
  runParser :: [s] -> ([s], Either e a)
}

data ParseError = ParseError String String
  deriving Show


-- BASIC PARSERS

any :: MPParser s ParseError s
any = MPParser $ \input -> case input of
  (x:xs) -> (xs, Right x)
  []     -> ([], Left $ (ParseError "any" "input consumed"))


{-
  > runParser (satisfy "starts with x" (\c -> c == 'x')) "xyz"
  ("yz",Right 'x')

  > runParser (satisfy "starts with x" (\c -> c == 'x')) "axyz"
  ("axyz", Left (ParseError "starts with x" "'a'"))
-}
satisfy :: String -> (a -> Bool) -> MPParser a ParseError a
satisfy description predicate = try $ do
  c <- MiniParsec.any
  if predicate c
    then return c
    else parseError description "satisfy"

eof :: MPParser x ParseError ()
eof = MPParser $ \input -> case input of
  -- no input left: the parser succeeds
  []    -> ([], Right ())
  -- leftover data: the parser fails
  (c:_) -> (input, Left $ ParseError "eof" "left over input!")


parseError :: String -> String -> MPParser x ParseError a 
parseError source description =
    MPParser $ \input -> (input, Left $ ParseError source description)
    

try :: MPParser x e a -> MPParser x e a
try p = MPParser $ \state -> case runParser p state of
  (_newState, Left err) -> (state, Left err)
  success               -> success



-- FUNCTOR, APPLICATIVE, AND MONAD

andThen :: MPParser x e a -> (a -> MPParser x e b) -> MPParser x e b
parserA `andThen` f = MPParser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)

(>>=) :: MPParser x e a -> (a -> MPParser x e b) -> MPParser x e b
(>>=) = andThen

fmap_ :: (a -> b) -> MPParser x e a -> MPParser x e b
fmap_ f p = MPParser $ \input -> 
       case runParser p input of 
           (input2, Left err) -> (input2, Left err)
           (input2, Right a) -> (input2, Right (f a))

ap :: MPParser x e (a -> b) -> MPParser x e a -> MPParser x e b 
ap pf p = 
  MPParser $ \input -> 
    (case runParser pf input of 
        (input2, Left err) -> (input2, Left err)
        (input2, Right f) -> 
            case runParser p input2 of 
                (input3, Left err) -> (input3, Left err)
                (input3, Right a) -> (input3, Right (f a)))

bind :: MPParser x e a -> (a -> MPParser x e b) -> MPParser x e b
bind parserA f = MPParser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left  e) -> (restOfInput, Left e)


instance Functor (MPParser x e) where
    -- fmap :: (a -> b) -> MPParser x a -> MPParser x b
    fmap f p = 
        MPParser $ \input ->
            case runParser p input of 
                (input2, Left err) -> (input2, Left err)
                (input2, Right a) -> (input2, Right (f a))


instance Applicative (MPParser x e) where 
    -- pure :: a -> MPParser a
    pure a = MPParser $ \input -> (input, Right a)
    -- (<*>) :: MPParser (a -> b) -> MPParser a -> MPParser b 
    (<*>) pf p = ap pf p


instance Monad (MPParser x e) where
    (>>=) = bind


-- COMBINATORS

(<|>) :: Eq x => MPParser x e a -> MPParser x e a -> MPParser x e a
p1 <|> p2 = MPParser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

choice :: Eq x => String -> [MPParser x ParseError a] -> MPParser x ParseError a
choice description ps = foldr (<|>) noMatch ps
  where noMatch = parseError'

parseError' :: MPParser x ParseError a 
parseError' =
    MPParser $ \input -> (input, Left $ ParseError "No match" "choice")


many, many1 :: Eq x => MPParser x e a -> MPParser x e [a]
many  p = many1 p <|> return []
many1 p = do
  first <- p
  rest  <- many p
  return (first:rest)


sepBy, sepBy1 :: Eq x => MPParser x e a -> MPParser x e s -> MPParser x e [a]
sepBy  p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest  <- many (s >> p)
  return (first:rest)


{-
Apply parser p, then apply parser function q as many times as possible.
For both p and q, accumulate the results.  This combinator is useful
when parsing expressions resulting from the production rule 

     a --> b c*

-}
manyP :: MPParser x e a -> (a -> MPParser x e a) -> MPParser x e a
manyP p q =
  MPParser $ \s -> case runParser p s of 
    (s', Left err) -> (s, Left err)
    (s'', Right a) -> runParser (manyP' a q) s''

manyP' :: a -> (a -> MPParser x e a) -> MPParser x e a
manyP' a q = 
  MPParser $ \s -> case runParser (q a) s of
        (s', Left err) -> (s, Right a)
        (s'', Right a'') -> runParser (manyP' a'' q) s''

{-
Like sequence, but for MPParser
-}
mpSequence :: [MPParser x e [a]] -> MPParser x e [a]
mpSequence (p:[]) = p
mpSequence (p:ps) = do
    first <- p
    rest <- mpSequence ps
    return (first ++ rest)


-- HELPERS

mapEither :: (a -> a') -> Either a b -> Either a' b
mapEither f e =
  case e of
    Left x -> Left (f x)
    Right y -> Right y

mapFirst :: (a -> b)->  (a,x) -> (b,x)
mapFirst f (a,x) = (f a, x)

mapSecond :: (x -> y)->  (a,x) -> (a,y)
mapSecond f (a,x) = (a, f x)

mapError :: (e -> e') -> MPParser s e a -> MPParser s e' a
mapError f p =
  MPParser $ \ss -> mapSecond (mapEither f) ((runParser p) ss)
