module Scanner where 

import Parser

data Token = Token { typ :: TokenType, lexeme :: String, tokenValue :: TokenValue, lineNumber :: Int} deriving Show

data TokenValue = TSymbol | TString String | TNumber Double deriving Show

data TokenType = LEFT_PAREN| RIGHT_PAREN| LEFT_BRACE| RIGHT_BRACE|
  COMMA| DOT| MINUS| PLUS| SEMICOLON| SLASH| STAR|
  -- One or two character tokens
  BANG| BANG_EQUAL|
  EQUAL| EQUAL_EQUAL|
  GREATER| GREATER_EQUAL|
  LESS| LESS_EQUAL|
  -- Literals
  IDENTIFIER| STRING| NUMBER|
  -- Keywords
  AND| CLASS| ELSE| FALSE| FUN| FOR| IF| NIL| OR|
  PRINT| RETURN| SUPER| THIS| TRUE| VAR| WHILE|
  EOF
  deriving Show


scan :: String -> [Token]
scan input = []

tokenParser k = choice "foo" [leftParen k, rightParen k, leftBrace k, rightBrace k, comma k, dot k, minus k, plus k, semicolon k, 
                             slash k, star k, bang k, bangEqual k, equal k, equalEqual k, greater k, greaterEqual k, less k, 
                             lessEqual k, keywordAnd k, keywordClass k, keywordElse k, keywordFalse k, keywordFun k, 
                             keywordFor k, keywordIf k, keywordNil k, keywordOr k, keywordPrint k, keywordReturn k, 
                             keywordSuper k, keywordThis k, keywordTrue k, keywordVar k, keywordWhile k,
                             identifier_ k, stringLiteral k, number k]


-- parseLine :: Int -> String -> 
parseLine k input = runParser (lineParser k) input


lineParser k = many (tokenParser k)

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

-- NUMBER
positiveNumber :: Int -> Parser Token
positiveNumber line_ = do
    x <- doubleDigits
    return Token { typ = NUMBER, lexeme = x, tokenValue = TNumber (read x),  lineNumber = line_ }


negativeNumber :: Int -> Parser Token
negativeNumber line_ = do
    x <- string "-" >> doubleDigits
    let x' = '-':x
    return Token { typ = NUMBER, lexeme = x', tokenValue = TNumber (read x'),  lineNumber = line_ }


number :: Int -> Parser Token
number line_ = choice "number" [try $ negativeNumber line_, positiveNumber line_]

identifier_ :: Int -> Parser Token
identifier_ line_ = (\s -> Token { typ = IDENTIFIER, lexeme = s, tokenValue = TString s,  lineNumber = line_ }) <$> identifier

stringLiteral :: Int -> Parser Token
stringLiteral line_ = (\s -> Token { typ = STRING, lexeme = s, tokenValue = TString s,  lineNumber = line_ }) <$>  literalString

-- AND
keywordAnd :: Int -> Parser Token
keywordAnd k = (\s -> Token { typ = AND, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "and")

--  CLASS
keywordClass :: Int -> Parser Token
keywordClass k = (\s -> Token { typ = CLASS, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "class")

--  ELSE
keywordElse :: Int -> Parser Token
keywordElse k = (\s -> Token { typ = ELSE, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "else")

-- FALSE 
keywordFalse :: Int -> Parser Token
keywordFalse k = (\s -> Token { typ = FALSE, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "false")

-- FUN
keywordFun :: Int -> Parser Token
keywordFun k = (\s -> Token { typ = FUN, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "fun")

-- FOR
keywordFor :: Int -> Parser Token
keywordFor k = (\s -> Token { typ = FOR, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "for")

-- IF
keywordIf :: Int -> Parser Token
keywordIf k = (\s -> Token { typ = IF, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "if")

-- NIL
keywordNil :: Int -> Parser Token
keywordNil k = (\s -> Token { typ = NIL, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "nil")


-- OR
keywordOr :: Int -> Parser Token
keywordOr k = (\s -> Token { typ = OR, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "or")

-- PRINT
keywordPrint :: Int -> Parser Token
keywordPrint k = (\s -> Token { typ = PRINT, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "print")

-- RETURN
keywordReturn :: Int -> Parser Token
keywordReturn k = (\s -> Token { typ = RETURN, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "return")

-- SUPER
keywordSuper :: Int -> Parser Token
keywordSuper k = (\s -> Token { typ = SUPER, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "super")

-- THIS
keywordThis :: Int -> Parser Token
keywordThis k = (\s -> Token { typ = THIS, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "this")

-- TRUE
keywordTrue :: Int -> Parser Token
keywordTrue k = (\s -> Token { typ = TRUE, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "true")

-- VAR
keywordVar :: Int -> Parser Token
keywordVar k = (\s -> Token { typ = VAR, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "var")

-- WHILE
keywordWhile:: Int -> Parser Token
keywordWhile k = (\s -> Token { typ = WHILE, lexeme = s, tokenValue = TString s,  lineNumber = k }) <$>  try (symbol "while")
