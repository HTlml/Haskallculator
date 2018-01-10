module Expression(
    Item (..),
    Expression,
    value,
    isValue,
    --showExpr,
    sample1
) where

data Item = Add | Dif | Mul | Value Integer | LeftP | RightP deriving Show

type Expression = [Item]

value :: Item -> Integer
value (Value a) = a
value _         = error "Unknown value"

isValue :: Item -> Bool
isValue (Value _) = True
isValue _         = False

-- will return true when evaluated isValue (sample1!!0)

sample1 :: Expression
sample1 = [ Value 5, Add, LeftP, Value 6, Dif,
            Value 2, RightP, Mul, Value 3]

    