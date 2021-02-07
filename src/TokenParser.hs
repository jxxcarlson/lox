module TokenParser where 

import Scanner (Token(..), TokenType(..), TokenValue(..))

t0 = Token {typ = NIL, lexeme = "nil ", tokenValue = TNIL, lineNumber = 0}
t1 = Token {typ = NUMBER, lexeme = "5", tokenValue = TNumber 5.0, lineNumber = 2}

-- > TP.runParser (TP.satisfy "looking for +" (\t -> typ t == STAR)) [TP.t2]
-- ([],Right (Token {typ = STAR, lexeme = "*", tokenValue = TSymbol, lineNumber = 2}))
--
-- > TP.runParser (TP.satisfy "looking for +" (\t -> typ t == STAR)) [TP.t1]
-- ([Token {typ = NUMBER, lexeme = "5", tokenValue = TNumber 5.0, lineNumber = 2}],Left (ParseError {lineNo = 0, message = "looking for +", tokens = []}))
t2 = Token {typ = STAR, lexeme = "*", tokenValue = TSymbol, lineNumber = 2}


newtype Parser a = Parser {
  runParser :: [Token] -> ([Token], Either ParseError a)
}

data ParseError = ParseError { lineNo :: Int, message :: String, tokens :: [Token] } deriving Show

any :: Parser Token
any = Parser $ \input -> case input of
  (x:xs) -> (xs, Right x)
  []     -> ([], Left $ ParseError { lineNo = 0, message = "", tokens = []})

satisfy :: String -> (Token -> Bool) -> Parser Token
satisfy description predicate = try $ do
  c <- TokenParser.any
  if predicate c
    then return c
    else parseError description c

parseError :: String -> a -> Parser a 
parseError description x =
    Parser $ \input -> (input, Left $ ParseError 0 description input)
   

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

many, many1 :: Parser a -> Parser [a]
many  p = many1 p <|> return []
many1 p = do
  first <- p
  rest  <- many p
  return (first:rest)

  
parseError' :: Parser a 
parseError' =
    Parser $ \input -> (input, Left $ TokenParser.ParseError {lineNo = 0, message = "No match", tokens = input})


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