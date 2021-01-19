module Parser where

import Scanner

newtype Parser a b = Parser {
  runParser :: [a] -> ([a], Either ParseError b)
}

number :: Parser Token Expression
number = Parser $ \input -> 
  let 
      (t:ts) = input
  in
      if typ t == NUMBER then
        (ts, Right (toLiteral $ tokenValue t))
        -- (ts, Right (makeNumber 1))
      else
      ([], Left $ ParseError {lineNo = Scanner.lineNumber t, message = "Error on NUMBER"})
   

toLiteral :: TokenValue -> Expression
toLiteral tv = 
    case tv of
        TSymbol -> Literal NIL_
        TString s -> Literal (STR s)
        TNumber x -> Literal (Number x)



data ParseError = ParseError { lineNo :: Int, message :: String} deriving Show

dummyError :: ParseError
dummyError = ParseError { lineNo = 0, message = "Nothing here yet"}

-- SYNTAX TREE

data Expression = Literal LiteralValue | Unary UnaryValue | Binary BinaryValue | Group Expression
   deriving Show 

data LiteralValue = Number Double | STR String | NIL_
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
        Literal litVal -> 
            case litVal of 
                Number x -> show x
                STR s -> s
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
makeNumber x = Literal (Number x)

makeString :: String -> Expression
makeString str = Literal (STR str)

expression :: [Token] -> Either ParseError Expression
expression tokens = Left dummyError

