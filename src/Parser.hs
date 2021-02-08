module Parser where

import Scanner (TokenType(..), TokenValue(..), Token(..), prettyPrint)
import TokenParser
    ( ParseError(..), Parser(Parser), runParser, satisfy, try, choice, many, many1 , (<|>))
import Control.Monad.Loops (concatM)

{- GRAMMAR

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

-}

-- SYNTAX TREE

data Expression = Primitive PrimitiveValue | Unary UnaryValue | Binary BinaryValue | Group Expression
   deriving Show 

data PrimitiveValue = Number Double | BoolVal Bool | STR String | NIL_ | UNIT
   deriving Show

data UnaryValue = UnaryValue {op :: UnaryOp, uexpr :: Expression}
   deriving Show

data BinaryValue = BinaryValue {leftExpr :: Expression, binop :: BinaryOp, rightExpr :: Expression}
   deriving Show

data BinaryOp = BEqual | BNotEqual | BLess | BLessEqual | BGreater | BGreaterEqual | BPlus | BMinus | BTimes | BDiv 
  deriving Show

data UnaryOp = UMinus | UBang 
  deriving Show


binaryOpOfToken :: Token -> BinaryOp
binaryOpOfToken tok = 
     case typ tok of 
       MINUS -> BMinus
       PLUS -> BPlus
       STAR -> BTimes
       SLASH -> BDiv
       EQUAL_EQUAL -> BEqual
       GREATER -> BGreater
       GREATER_EQUAL -> BGreaterEqual
       LESS -> BLess
       LESS_EQUAL -> BLessEqual



fooM :: Monad m => m a -> [a -> m a] -> m a
fooM ma [] = ma
-- fooM ma (f:fs) = fooM ma fs >>= f 
-- fooM ma (f:fs) = fooM (ma >>= f) fs
fooM ma fs = foldl (>>=) ma fs

-- barM :: Monad m => [a -> m a] -> (a -> m a)
-- barM [f] = f
-- barM (f:fs) = (\x -> x >>= f >>= barM fs)
 

-- TOP LEVEL PARSER

applyMany :: Parser a -> (a -> Parser a) -> Parser a
applyMany p fp =
  Parser $ \s -> case runParser p s of 
    (s', Left err) -> (s, Left err)
    (s'', Right a) -> runParser (applyMany' a fp) s''

applyMany' :: a -> (a -> Parser a) -> Parser a
applyMany' a fp = 
  Parser $ \s -> case runParser (fp a) s of
        (s', Left err) -> (s, Right a)
        (s'', Right a'') -> runParser (applyMany' a'' fp) s''

expression :: Parser Expression
expression = term

-- BINARY

term :: Parser Expression
term = applyMany factor termWith


termWith :: Expression -> Parser Expression
termWith expr = do
    op <- termOp
    u <- factor
    return (Binary $ BinaryValue {leftExpr = expr, binop = binaryOpOfToken op, rightExpr = u})

termOp :: Parser Token
termOp = TokenParser.choice "expecting  termOp" [ plus, minus]


factor :: Parser Expression
-- factor = unary >>= concatM [factorWith]
-- factor = unary >>= factorWith --  >>= factorWith
-- factor = fooM unary [factorWith]
-- factor = try (unary >>= factorWith) <|> try unary
-- factor = oneOf [unary, unary >>= factorWith]
factor = applyMany unary factorWith


factorWith :: Expression -> Parser Expression
factorWith expr = do
    op <- factorOp
    u <- unary
    return (Binary $ BinaryValue {leftExpr = expr, binop = binaryOpOfToken op, rightExpr = u})

factorOp :: Parser Token
factorOp = TokenParser.choice "expecting factorOp" [ times, slash]

-- UNARY

unary :: Parser Expression
unary = choice "unary" [unaryOp >>= unary_, primary]

unaryOp :: Parser Token
unaryOp = choice "unaryOp" [uminus, bang]

unary_ :: Token -> Parser Expression
unary_ token = unaryMapper token <$> unary

unaryMapper :: Token -> Expression -> Expression
unaryMapper tok expr = case typ tok of 
  UMINUS -> Unary UnaryValue { op = UMinus, uexpr = expr}
  BANG  -> Unary UnaryValue { op = UBang, uexpr = expr}

uminusOp :: Parser Token
uminusOp = choice "uminusOp" [uminus, bang]

uminus :: Parser Token
uminus = satisfy "uminus, expecting -" (\t -> typ t == UMINUS)

bang :: Parser Token
bang = satisfy "bang, expecting !" (\t -> typ t == BANG)

plus :: Parser Token
plus = satisfy "plus, expecting +" (\t -> typ t == PLUS)

minus :: Parser Token
minus = satisfy "minus, expecting -" (\t -> typ t == MINUS)


times :: Parser Token
times = satisfy "times, expecting *" (\t -> typ t == STAR)

slash :: Parser Token
slash = satisfy "slash, expecting /" (\t -> typ t == SLASH)

-- PRIMARY

primary :: Parser Expression
primary = TokenParser.choice "group or primary" [try group, primitive]

group :: Parser Expression 
group = fmap Group ( skip LEFT_PAREN >> expression <* (skip RIGHT_PAREN) )   

primitive :: Parser Expression
primitive = Parser $ \input -> 
  case input of 
    [] -> ([], Left $ ParseError {lineNo = -1, message = "empty input", tokens = []})
    (t:ts) ->
      if typ t == NUMBER then
        (ts, Right (toPrimitive $ tokenValue t))
      else if typ t == STRING then
        (ts, Right (toPrimitive $ tokenValue t))
      else if typ t == TRUE then
        (ts, Right (toPrimitive $ tokenValue t))
      else if typ t == FALSE then
        (ts, Right (toPrimitive $ tokenValue t))
      else if typ t == NIL then
        (ts, Right (toPrimitive $ tokenValue t))
      else
       (ts, Left $ ParseError {lineNo = Scanner.lineNumber t, message = "Expecting primitive", tokens = input})

-- HELPERS

skip :: TokenType -> Parser Expression
skip tt = Parser $ \input ->
    let 
        (t:ts) = input
    in
        if typ t == tt then
            (ts, Right (Primitive UNIT))
        else
            (ts, Left ParseError {lineNo = Scanner.lineNumber t, message = "Expecting " ++ show tt ++ ", actual = " ++ show (typ t), tokens = input})

toPrimitive :: TokenValue -> Expression
toPrimitive tv = 
    case tv of
        TSymbol -> Primitive NIL_
        TString s -> Primitive (STR s)
        TNumber x -> Primitive (Number x)
        TBool b -> Primitive (BoolVal b)
        TNIL -> Primitive NIL_




dummyError :: ParseError
dummyError = ParseError { lineNo = 0, message = "Nothing here yet", tokens = []}

-- PRETTY PRINTER

{-

> e1 = Binary (BinaryValue {leftExpr = makeNumber 2, binop = BPlus, rightExpr = makeNumber 3})
> putStrLn $ Parser.prettyPrint (Group e1)
(2.0 + 3.0)

-}
prettyPrint :: Expression -> String
prettyPrint expr = 
    case expr of 
        Primitive primVal -> 
            case primVal of 
                Number x -> show x
                STR s -> s
                BoolVal b -> show b
                NIL_ -> "nil"
        Unary uVal -> prettyPrintUnaryOp (op uVal) ++ Parser.prettyPrint (uexpr uVal)
        Binary bVal -> Parser.prettyPrint (leftExpr bVal) ++ " " ++ prettyPrintBinop (binop bVal) ++ " " ++ Parser.prettyPrint (rightExpr bVal)
        Group e -> "(" ++ Parser.prettyPrint e ++ ")"

           

prettyPrintUnaryOp :: UnaryOp -> String
prettyPrintUnaryOp o = 
  case o of 
    UMinus -> "-"
    UBang -> "!"

prettyPrintBinop :: BinaryOp -> String 
prettyPrintBinop binop = 
    case binop of 
        BEqual -> "=="
        BNotEqual -> "!="
        BLess -> "<"
        BLessEqual -> "<="
        BGreater -> ">"
        BGreaterEqual -> ">="
        BPlus -> "+"
        BMinus -> "-"
        BTimes -> "*"
        BDiv -> "/"



makeNumber :: Double -> Expression
makeNumber x = Primitive (Number x)

makeString :: String -> Expression
makeString str = Primitive (STR str)

