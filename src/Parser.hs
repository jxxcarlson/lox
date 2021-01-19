module Parser where

data Expression = Literal LiteralValue | Unary UnaryValue | Binary BinaryValue | Group Expression
   deriving Show 

data LiteralValue = Number Double | STR String 
   deriving Show

data UnaryValue = UnaryValue {op :: UnaryOp, uexpr :: Expression}
   deriving Show

data BinaryValue = BinaryValue {leftExpr :: Expression, binop :: BinaryOp, rightExpr :: Expression}
   deriving Show

data BinaryOp = BEqual | BNotEqual | BLess | BLessEqual | BGreater | BGreaterEqual | BPlus | BMinus | BTimes | BDiv 
  deriving Show

data UnaryOp = UMinus | UBang 
  deriving Show

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
        Unary uVal -> prettyPrintUnaryOp (op uVal) ++ prettyPrint (uexpr uVal)
        Binary bVal -> prettyPrint (leftExpr bVal) ++ " " ++ prettyPrintBinop (binop bVal) ++ " " ++ prettyPrint (rightExpr bVal)
        Group e -> "(" ++ prettyPrint e ++ ")"

           

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

