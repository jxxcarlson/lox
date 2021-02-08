module Parser where

import Scanner (TokenType(..), TokenValue(..), Token(..), prettyPrint)


-- import TokenParser
--     ( TParseError(..), TokenParser(TokenParser), runParser, satisfy, try, choice, many, many1 , (<|>))

import Control.Monad.Loops (concatM)

import MiniParsec as TokenParser

type TokenParser = MPParser Token

data TParseError = TParseError { lineNo :: Int, message :: String, tokens :: [Token] } deriving Show


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
       BANG_EQUAL -> BNotEqual
       GREATER -> BGreater
       GREATER_EQUAL -> BGreaterEqual
       LESS -> BLess
       LESS_EQUAL -> BLessEqual



-- TOP LEVEL PARSER


expression :: TokenParser Expression
expression = equality


manyP :: TokenParser a -> (a -> TokenParser a) -> TokenParser a
manyP p fp =
  TokenParser $ \s -> case runParser p s of 
    (s', Left err) -> (s, Left err)
    (s'', Right a) -> runParser (manyP' a fp) s''

manyP' :: a -> (a -> TokenParser a) -> TokenParser a
manyP' a fp = 
  TokenParser $ \s -> case runParser (fp a) s of
        (s', Left err) -> (s, Right a)
        (s'', Right a'') -> runParser (manyP' a'' fp) s''


-- EQUALITY

equality :: TokenParser Expression
equality = manyP comparison equalityWith

equalityOp = TokenParser.choice "expecting equalityOperator" [equal, notEqual]

equalityWith :: Expression -> TokenParser Expression
equalityWith expr = do
    op <- equalityOp
    u <- comparison
    return (Binary $ BinaryValue {leftExpr = expr, binop = binaryOpOfToken op, rightExpr = u})

-- COMPARISON

comparison :: TokenParser Expression 
comparison = manyP term comparisonWith

comparisonWith :: Expression -> TokenParser Expression
comparisonWith expr = do
    op <- comparisonOp
    u <- term
    return (Binary $ BinaryValue {leftExpr = expr, binop = binaryOpOfToken op, rightExpr = u})

comparisonOp = TokenParser.choice "expecting comparsionOperator" [lessThan, lessThanOrEqual, greaterThan, greaterThanOrEqual]

-- TERM

term :: TokenParser Expression
term = manyP factor termWith


termWith :: Expression -> TokenParser Expression
termWith expr = do
    op <- termOp
    u <- factor
    return (Binary $ BinaryValue {leftExpr = expr, binop = binaryOpOfToken op, rightExpr = u})

termOp :: TokenParser Token
termOp = TokenParser.choice "expecting  termOp" [ plus, minus]


factor :: TokenParser Expression
factor = manyP unary factorWith


factorWith :: Expression -> TokenParser Expression
factorWith expr = do
    op <- factorOp
    u <- unary
    return (Binary $ BinaryValue {leftExpr = expr, binop = binaryOpOfToken op, rightExpr = u})

factorOp :: TokenParser Token
factorOp = TokenParser.choice "expecting factorOp" [ times, slash]

-- UNARY

unary :: TokenParser Expression
unary = choice "unary" [unaryOp >>= unary_, primary]

unaryOp :: TokenParser Token
unaryOp = choice "unaryOp" [uminus, bang]

unary_ :: Token -> TokenParser Expression
unary_ token = unaryMapper token <$> unary

unaryMapper :: Token -> Expression -> Expression
unaryMapper tok expr = case typ tok of 
  UMINUS -> Unary UnaryValue { op = UMinus, uexpr = expr}
  BANG  -> Unary UnaryValue { op = UBang, uexpr = expr}

equal :: TokenParser Token
equal = satisfy "equal, expecting ==" (\t -> typ t == EQUAL_EQUAL)

notEqual :: TokenParser Token
notEqual = satisfy "not equal, expecting !=" (\t -> typ t == BANG_EQUAL)


greaterThan :: TokenParser Token
greaterThan = satisfy "greater than, expecting >" (\t -> typ t == GREATER)

greaterThanOrEqual :: TokenParser Token
greaterThanOrEqual = satisfy "greater than or equal, expecting >=" (\t -> typ t == GREATER_EQUAL)

lessThan :: TokenParser Token
lessThan = satisfy "less than, expecting <" (\t -> typ t == LESS)

lessThanOrEqual :: TokenParser Token
lessThanOrEqual = satisfy "less than or equal, expecting <=" (\t -> typ t == LESS_EQUAL)

uminusOp :: TokenParser Token
uminusOp = choice "uminusOp" [uminus, bang]

uminus :: TokenParser Token
uminus = satisfy "uminus, expecting -" (\t -> typ t == UMINUS)

bang :: TokenParser Token
bang = satisfy "bang, expecting !" (\t -> typ t == BANG)

plus :: TokenParser Token
plus = satisfy "plus, expecting +" (\t -> typ t == PLUS)

minus :: TokenParser Token
minus = satisfy "minus, expecting -" (\t -> typ t == MINUS)


times :: TokenParser Token
times = satisfy "times, expecting *" (\t -> typ t == STAR)

slash :: TokenParser Token
slash = satisfy "slash, expecting /" (\t -> typ t == SLASH)

-- PRIMARY

primary :: TokenParser Expression
primary = TokenParser.choice "group or primary" [try group, primitive]

group :: TokenParser Expression 
group = fmap Group ( skip LEFT_PAREN >> expression <* (skip RIGHT_PAREN) )   

primitive :: TokenParser Expression
primitive = TokenParser $ \input -> 
  case input of 
    [] -> ([], Left $ TParseError {lineNo = -1, message = "empty input", tokens = []})
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
       (ts, Left $ TParseError {lineNo = Scanner.lineNumber t, message = "Expecting primitive", tokens = input})

-- HELPERS

skip :: TokenType -> TokenParser Expression
skip tt = TokenParser $ \input ->
    let 
        (t:ts) = input
    in
        if typ t == tt then
            (ts, Right (Primitive UNIT))
        else
            (ts, Left TParseError {lineNo = Scanner.lineNumber t, message = "Expecting " ++ show tt ++ ", actual = " ++ show (typ t), tokens = input})

toPrimitive :: TokenValue -> Expression
toPrimitive tv = 
    case tv of
        TSymbol -> Primitive NIL_
        TString s -> Primitive (STR s)
        TNumber x -> Primitive (Number x)
        TBool b -> Primitive (BoolVal b)
        TNIL -> Primitive NIL_




dummyError :: TParseError
dummyError = TParseError { lineNo = 0, message = "Nothing here yet", tokens = []}

-- PRETTY PRINTER

{-

> e1 = Binary (BinaryValue {leftExpr = makeNumber 2, binop = BPlus, rightExpr = makeNumber 3})
> putStrLn $ TokenParser.prettyPrint (Group e1)
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
        Unary uVal -> prettyPrintUnaryOp (op uVal) ++ TokenParser.prettyPrint (uexpr uVal)
        Binary bVal -> TokenParser.prettyPrint (leftExpr bVal) ++ " " ++ prettyPrintBinop (binop bVal) ++ " " ++ TokenParser.prettyPrint (rightExpr bVal)
        Group e -> "(" ++ TokenParser.prettyPrint e ++ ")"

           

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



fooM :: Monad m => m a -> [a -> m a] -> m a
fooM ma [] = ma
fooM ma fs = foldl (>>=) ma fs



