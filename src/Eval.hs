module Eval where

import Parser (Expression(..)
            , PrimitiveValue(..)
            , UnaryValue (..) 
            , BinaryValue(..)
            , UnaryOp(..)
            , BinaryOp(..))



eval :: Expression -> Maybe PrimitiveValue 
eval expr = 
    case expr of
        Primitive val -> Just val
        Unary val -> 
           case op val of 
               UMinus -> eval (uexpr val) >>= applyUMinus
               UBang -> eval (uexpr val) >>= applyNot
        Binary val ->
            case binop val of 
                BPlus -> liftDoubleOp (+) (eval (leftExpr val)) (eval (rightExpr val))
                BMinus -> liftDoubleOp (-)  (eval (leftExpr val)) (eval (rightExpr val))
                BTimes -> liftDoubleOp (*)  (eval (leftExpr val)) (eval (rightExpr val))
                BDiv -> liftDoubleOp (/)  (eval (leftExpr val)) (eval (rightExpr val))
                BEqual -> liftPredicate (==)  (eval (leftExpr val)) (eval (rightExpr val))
                BNotEqual -> liftPredicate (/=)  (eval (leftExpr val)) (eval (rightExpr val))
                BLess -> liftPredicate (<)  (eval (leftExpr val)) (eval (rightExpr val))
                BLessEqual -> liftPredicate (<=)  (eval (leftExpr val)) (eval (rightExpr val))
                BGreater -> liftPredicate (>)  (eval (leftExpr val)) (eval (rightExpr val))
                BGreaterEqual -> liftPredicate (>=)  (eval (leftExpr val)) (eval (rightExpr val))                         
        Group expr -> eval expr



liftDoubleOp:: (Double -> Double -> Double) -> Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
liftDoubleOp op p q = 
    case (p, q) of 
        (Just (Number x), Just (Number y)) -> Just (Number (x `op` y))
        _ -> Nothing

liftBoolOp:: (Bool -> Bool -> Bool) -> Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
liftBoolOp op p q = 
    case (p, q) of 
        (Just (BoolVal x), Just (BoolVal y)) -> Just (BoolVal (x `op` y))
        _ -> Nothing

liftPredicate:: (Double -> Double -> Bool) -> Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
liftPredicate op p q = 
    case (p, q) of 
        (Just (Number x), Just (Number y)) -> Just (BoolVal (x `op` y))
        _ -> Nothing


applyTimes :: Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
applyTimes p q = 
    case (p,q) of 
        ((Just (Number x)), (Just (Number y))) -> Just (Number (x * y))
        _ -> Nothing

applyDiv:: Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
applyDiv p q = 
    case (p,q) of 
        ((Just (Number x)), (Just (Number y))) -> Just (Number (x / y))
        _ -> Nothing 

applyPlus :: Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
applyPlus p q = 
    case (p,q) of 
        ((Just (Number x)), (Just (Number y))) -> Just (Number (x + y))
        _ -> Nothing

applyMinus :: Maybe PrimitiveValue -> Maybe PrimitiveValue -> Maybe PrimitiveValue
applyMinus p q = 
    case (p,q) of 
        ((Just (Number x)), (Just (Number y))) -> Just (Number (x - y))
        _ -> Nothing        

applyUMinus :: PrimitiveValue -> Maybe PrimitiveValue
applyUMinus p = 
    case p of 
        Number x -> Just (Number (-x))
        _ -> Nothing
        
applyNot :: PrimitiveValue -> Maybe PrimitiveValue
applyNot p = 
    case p of 
        BoolVal b -> Just (BoolVal (not b))
        _ -> Nothing


-- data Expression = Primitive PrimitiveValue | Unary UnaryValue | Binary BinaryValue | Group Expression
--    deriving Show

-- data PrimitiveValue = Number Double | BoolVal Bool | STR String | NIL_ | UNIT
--    deriving Show

-- data UnaryValue = UnaryValue {op :: UnaryOp, uexpr :: Expression}
--    deriving Show

-- data BinaryValue = BinaryValue {leftExpr :: Expression, binop :: BinaryOp, rightExpr :: Expression}
--    deriving Show

-- data BinaryOp = BEqual | BNotEqual | BLess | BLessEqual | BGreater | BGreaterEqual | BPlus | BMinus | BTimes | BDiv 
--   deriving Show

-- data UnaryOp = UMinus | UBang 
--   deriving Show
