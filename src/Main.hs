module Main where
import Numeric
import Data.Char
import Data.Bits
import Text.Show
import GHC.Word
main::IO()
main = undefined

-- DEPRECATED

-- Описываем тип оператор (оператор - любое действие)
data Operator = 
        Plus Int |
        Minus Int |
        Mult Int |
        Feistel Int [Operator] |
        PBlock [Int] |
        SBlock [Int]
        
-- Применяем список функций к числу и возвращаем результат
evalOps :: Int -> [Operator] -> Int
evalOps x [] = x
evalOps x (h:t) = evalOps (evalOp x h) t

-- вычисляем выражение типа 3 (PLUS 5)
evalOp :: Int -> Operator -> Int
-- вычисляем +
evalOp x (Plus y)  = x + y
-- вычисляем -
evalOp x (Minus y)  = x - y
-- вычисляем *
evalOp x (Mult y) = x * y
-- останавливаем рекурсию при вычислении схемы Фейстеля
evalOp x (Feistel 0 _) = x
-- рекурсивный переход для вычисления схемы Фейстеля
evalOp x (Feistel count operators) =
        evalOp (evalOps x operators) (Feistel (count - 1) operators) 
evalOp x (PBlock pblock) = 
        evalPBlock x pblock
evalOp x (SBlock sblock) =
        evalSBlock x sblock

  
evalPBlock :: Int -> [Int] -> Int
evalPBlock input pblock =
       foldl (+) 0 (zipWith (*) 
       [if (testBit input (pblock !! i)) then 1 else 0 | i <- [0..((length pblock) - 1)]] 
       [2^i | i <- [0..((length pblock) - 1)]])
        
-- TODO: Вычислить S-блок, у которого для каждого элемента, своя таблица
-- evalSBlock :: Int -> [[Int]] -> Int
-- Нужно для ГОСТа        
        
-- Разбиваем число на массив чисел длина которых в битах равна длине s-блока.
-- TODO: кидаеть Exception если длина сблока не степень 2-ки.
separate :: Int -> [Int] -> [Int]
separate input sblock =
        [rem (quot input (length sblock)^i) (length sblock) | i <- [0..((quot (bitSize input) (length sblock)) - 1)]]

evalSBlock :: Int -> [Int] -> Int
evalSBlock input sblock =
        foldl (+) 0
        (zipWith (*)
        [sblock !! x | x <- separate input sblock]
        [(length sblock)^i | i <- [0.. (quot (bitSize input) (length sblock))]])

makeBitArray :: Int -> [Int]        
makeBitArray integer =
        [if (testBit integer x) then 1 else 0 | x <- [0..((bitSize integer) - 1)]]  

--compressBitArray :: [Int] -> Int -> [Int]
--compressBitArray bits power =
--        [(take power bits), last (splitAt power bits)]
        

-- вычисляем выражение типа 3 [(PLUS 5),(PLUS 6),(PLUS 7)]
evalExp :: Int -> [Operator] -> Int
evalExp x [] = x
evalExp x (y:ys) = evalExp (evalOp x y) ys 



        