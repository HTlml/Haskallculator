--------------------------------------------------------------------------------
-- Made by Alejandro Garau Madrigal
--------------------------------------------------------------------------------
module InFix (
    evaluateInFix
) where

import qualified DataStructures.Stack.LinearStack as Data -- Stack for data
import qualified DataStructures.Stack.LinearStack as Op   -- Stack for operations
import WellBalanced
import Expression

evaluateInFix :: Expression -> Integer
evaluateInFix expr  =
    if isExprBalanced expr then aux expr Data.empty Op.empty
        else error "Not a balanced expression."
    where
        aux []  dat op            = Data.top dat
        aux ((Value p):xs) dat op = aux xs (Data.push p dat) op
        aux ((Add):xs) dat op     = aux xs dat (Op.push '+' op)
        aux ((Dif):xs) dat op     = aux xs dat (Op.push '-' op)
        aux ((Mul):xs) dat op     = aux xs dat (Op.push '*' op)
        aux ((LeftP):xs) dat op   = aux xs dat op
        aux ((RightP):xs) dat op  = aux xs dat' op'
            where
                dat' = evaluate dat op
                op' = Op.pop op

evaluate :: (Num a) => Data.Stack a -> Op.Stack Char -> Data.Stack a
evaluate dat op
    | Op.top op == '+' = Data.push ((dato1) + (dato2)) dat'
    | Op.top op == '-' = Data.push ((dato1) - (dato2)) dat'
    | Op.top op == '*' = Data.push ((dato1) * (dato2)) dat'
        where
            dato1 = Data.top dat
            dato2 = Data.top (Data.pop dat)
            dat'  = Data.pop (Data.pop dat)

expression :: Expression
expression = [LeftP, Value 3, Mul, Value 5, RightP]
