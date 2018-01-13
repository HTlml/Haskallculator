--------------------------------------------------------------------------------
-- Estructuras de Datos. 2o Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería del Software
-- Alumno: GARAU MADRIGAL, Alejandro
--
--------------------------------------------------------------------------------

module WellBalanced where

-- Pushdown automaton where the stack alphabet is {'(', ')', '{', '}', '[', ']'}
-- and the input alphabet is the unicode table.

import qualified DataStructures.Stack.LinearStack as S
import Expression

wellBalanced :: String -> Bool
wellBalanced xs = wellBalanced' xs S.empty

wellBalanced' :: String -> S.Stack Char -> Bool
wellBalanced' [] s      = S.isEmpty s
wellBalanced' (x:xs) s
    | x == '('  = wellBalanced' xs (S.push x s)
    | x == '{'  = wellBalanced' xs (S.push x s)
    | x == '['  = wellBalanced' xs (S.push x s)
    | x == ')' && S.isEmpty s = False
    | x == '}' && S.isEmpty s = False
    | x == ']' && S.isEmpty s = False
    | x == ')' && S.top s == '(' = wellBalanced' xs (S.pop s)
    | x == '}' && S.top s == '{' = wellBalanced' xs (S.pop s)
    | x == ']' && S.top s == '[' = wellBalanced' xs (S.pop s)
    | otherwise = wellBalanced' xs s

isExprBalanced :: Expression -> Bool
isExprBalanced expr = isExprBalanced' expr S.empty

isExprBalanced' :: Expression -> S.Stack Item -> Bool
isExprBalanced' [] s        = S.isEmpty s
isExprBalanced' (x:xs) s
    | x == LeftP  = isExprBalanced' xs (S.push x s)
    | x == RightP && S.isEmpty s = False
    | x == RightP && S.top s == LeftP = isExprBalanced' xs (S.pop s)
    | otherwise = isExprBalanced' xs s

expr :: Expression
expr = [LeftP, Value 4, Add, Value 5]
