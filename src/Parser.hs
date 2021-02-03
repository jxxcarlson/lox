module Parser where

import Scanner (TokenType(..), TokenValue(..), Token(..), prettyPrint)
import TokenParser


unary :: Parser Expression
unary = choice "foo" [(unaryHelper <$> uminusOp), primary]

  --uminusOp >>= (unary <|> primitive) . unaryHelper
  -- (unaryHelper <$> uminusOp) >>= unary <|> primitive



-- basicUnary :: Parser Expression
-- basicUnary = uminusOp >>= (\tok -> ) 


unaryHelper :: Token -> Expression
unaryHelper tok = case typ tok of 
  UMINUS -> Unary UnaryValue { op = UMinus, uexpr = Primitive $ Number 3.20}
  BANG  -> Unary UnaryValue { op = UBang, uexpr = Primitive $ BoolVal True}

expression :: Parser Expression
expression = unary


-- unary = ( "!" | "-" ) unary | primary ;

uminusOp :: Parser Token
uminusOp = choice "uminusOp" [uminus, bang]

factorOp :: Parser Token
factorOp = TokenParser.choice "expecting factorOp" [ times, slash]

uminus :: Parser Token
uminus = satisfy "uminus, expecting -" (\t -> typ t == UMINUS)

bang :: Parser Token
bang = satisfy "bang, expecting !" (\t -> typ t == BANG)

times :: Parser Token
times = satisfy "times, expecting +" (\t -> typ t == STAR)

slash :: Parser Token
slash = satisfy "times, expecting /" (\t -> typ t == SLASH)



primary :: Parser Expression
primary = TokenParser.choice "group or primary" [try primitive, group]

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

-- HELPER    
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

-- PARSER




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

