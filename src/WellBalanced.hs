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

wellBalanced :: String -> Bool
wellBalanced xs = wellBalanced' xs S.empty

wellBalanced' :: String -> S.Stack Char -> Bool
wellBalanced' [] s      = S.isEmpty s
wellBalanced' (x:xs) s  | x == '('  = wellBalanced' xs (S.push x s)
                        | x == '{'  = wellBalanced' xs (S.push x s)
                        | x == '['  = wellBalanced' xs (S.push x s)
                        | x == ')' && S.isEmpty s = False
                        | x == '}' && S.isEmpty s = False
                        | x == ']' && S.isEmpty s = False
                        | x == ')' && S.top s == '(' = wellBalanced' xs (S.pop s)
                        | x == '}' && S.top s == '{' = wellBalanced' xs (S.pop s)
                        | x == ']' && S.top s == '[' = wellBalanced' xs (S.pop s)
                        | otherwise = wellBalanced' xs s

kek :: String -> Int
kek cad = if not (wellBalanced cad) then error "Syntax error: not a balanced operation"
    else True
