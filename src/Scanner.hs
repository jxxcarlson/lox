module Scanner where 

import Parser

data Token = Token { typ :: TokenType, lexeme :: String, tokenValue :: TokenValue, lineNumber :: Int} deriving Show

data TokenValue = TSymbol | TString String | TDouble Double deriving Show

data TokenType = LEFT_PAREN| RIGHT_PAREN| LEFT_BRACE| RIGHT_BRACE|
  COMMA| DOT| MINUS| PLUS| SEMICOLON| SLASH| STAR|
  -- One or two character tokens.
  BANG| BANG_EQUAL|
  EQUAL| EQUAL_EQUAL|
  GREATER| GREATER_EQUAL|
  LESS| LESS_EQUAL|
  -- Literals.
  IDENTIFIER| STRING| NUMBER|
  -- Keywords.
  AND| CLASS| ELSE| FALSE| FUN| FOR| IF| NIL| OR|
  PRINT| RETURN| SUPER| THIS| TRUE| VAR| WHILE|
  EOF
  deriving Show


scan :: String -> [Token]
scan input = []

parseToken k = choice "foo" [leftParen k, rightParen k, leftBrace k, rightBrace k, comma k, dot k, minus k, plus k, semicolon k, 
                             slash k, star k, bang k, bangEqual k, equal k, equalEqual k, greater k, greaterEqual k, less k, 
                             lessEqual k]

parseLine k = many (parseToken k)

-- PARSERS FOR INDIVIDUAL TOKENS





-- LEFT_PAREN
leftParen :: Int -> Parser Token
leftParen line_ = (\s -> Token { typ = LEFT_PAREN, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "("

-- RIGHT_PAREN
rightParen :: Int -> Parser Token
rightParen line_ = (\s -> Token { typ = RIGHT_PAREN, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol ")"

-- LEFT_BRACE
leftBrace :: Int -> Parser Token
leftBrace line_ = (\s -> Token { typ = LEFT_BRACE, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "["

-- RIGHT_BRACE
rightBrace :: Int -> Parser Token
rightBrace line_ = (\s -> Token { typ = RIGHT_BRACE, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "]"

-- COMMA
comma:: Int -> Parser Token
comma line_ = (\s -> Token { typ = COMMA, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol ","

-- DOT
dot :: Int -> Parser Token
dot line_ = (\s -> Token { typ = DOT, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "."

-- MINUS
minus :: Int -> Parser Token
minus line_ = (\s -> Token { typ = MINUS, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "-"

-- PLUS
plus :: Int -> Parser Token
plus line_ = (\s -> Token { typ = PLUS, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "+"

-- SEMICOLON
semicolon :: Int -> Parser Token
semicolon line_ = (\s -> Token { typ = SEMICOLON, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol ";"

-- SLASH
slash :: Int -> Parser Token
slash line_ = (\s -> Token { typ = SLASH, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "/"

--  STAR
star :: Int -> Parser Token
star line_ = (\s -> Token { typ = SLASH, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "*"

--  BANG
bang :: Int -> Parser Token
bang line_ = (\s -> Token { typ = BANG, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "!"

--  BANG_EQUAL
bangEqual :: Int -> Parser Token
bangEqual line_ = (\s -> Token { typ = SLASH, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "*"

--  EQUAL| EQUAL_EQUAL| GREATER| GREATER_EQUAL| LESS| LESS_EQUAL|
equal :: Int -> Parser Token
equal line_ = (\s -> Token { typ = EQUAL, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "="

--  EQUAL_EQUAL
equalEqual :: Int -> Parser Token
equalEqual line_ = (\s -> Token { typ = EQUAL_EQUAL, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "=="

--  GREATER
greater :: Int -> Parser Token
greater line_ = (\s -> Token { typ = GREATER, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol ">"

--  GREATER_EQUAL
greaterEqual :: Int -> Parser Token
greaterEqual line_ = (\s -> Token { typ = GREATER_EQUAL, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol ">="

--  LESS
less :: Int -> Parser Token
less line_ = (\s -> Token { typ = LESS, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "<"

--  LESS_EQUAL
lessEqual :: Int -> Parser Token
lessEqual line_ = (\s -> Token { typ = LESS_EQUAL, lexeme = s, tokenValue = TSymbol,  lineNumber = line_ }) <$> symbol "<="

-- IDENTIFIER| STRING| NUMBER|
