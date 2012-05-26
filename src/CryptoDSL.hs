module CryptoDSL where
import Data.Word
import Data.Bits
import Feistel

-- Operator - единица представления функции
data Operator = 
                -- Сложение с заданным словом
                Xor Word32 |
                -- Циклический сдвиг влево на I позиций
                RotateL Int |
                -- Циклический сдвиг вправо на I позиций
                RotateR Int |
                -- P-блок, заданный следующим образом: на i ой позиции результирующего слова будет стоять бит,
                -- стоящий на позиции [Int] !! i в исходном слове 
                PBlock [Int] |
                -- S-блок, заданный следующим образом: слово разбивается на заданное число блоков по 32/i бит, а затем производится
                -- замена с помощью одного блока замен.
                -- Блок замен задан следующим образом: если блок разбиения слова == i, то вместо этого блока в результирующее слово
                -- будет подставлен блок стоящий на i-ой позиции в блоке замен
                SBlock Int [Word32] |
                -- S-блок, аналогичный предыдущему, только с одним отличием: замена производится с помощью нескольких блоков замен
                -- (по одному на каждый блок разбиения)
                SBlocks Int [[Word32]] |
                -- Позволяет выполнить произвольную функцию, заданную на языке Haskell, параметром которого будет являться текущее значения вычисляемой функции
--                Custom (Word32 -> Word32) |
                -- Позволяет выполнить произвольную функцию, заданную на языке Haskell, вторым параметром которой яв-ся раундовый ключ k.
--                CustomKey (Word32 -> Word32 -> Word32) |
                -- Означает, что на данном шаге нужно к текущему значению прибавить значения раундового ключа по модулю 2
                XorKey |
                -- Означает, что на данном шаге нужно к текущему значению прибавить значения раундового ключа по модулю 2^32
                SumKey 
        deriving (Show, Read)

-- FeistelFunction реализует способ задания функции f в схеме Фейстеля через набор операторов
data FeistelFunction = FeistelFunction [Operator]  
        deriving (Show, Read)

-- Вспомогательная функция, приводящаю функцию f к необходимой сигнатуре
wrapF :: (Word32 -> Word32 -> Word32) -> (Word32 -> Word64 -> Word32)
wrapF function = \ a k -> function a (fromIntegral k)

-- Преобразует заданную с помощью набора операторов функцию f в функцию на языке Haskell
parseF :: FeistelFunction -> (Word32 -> Word32 -> Word32)
parseF (FeistelFunction operators) =
        foldl superposition same operators
        where
                superposition :: (Word32 -> Word32 -> Word32) -> Operator -> (Word32->Word32->Word32)
                superposition f XorKey = \ a k -> xor (f a k) k
--                superposition f (CustomKey g) = \ a k -> g (f a k) k
                superposition f SumKey = \ a k -> (f a k) + k 
                superposition f g = \ a k -> evalOp (f a k) g  
                same :: Word32 -> Word32 -> Word32 
                same arg _ = arg
                
evalOp :: Word32 -> Operator -> Word32
evalOp arg1 (Xor arg2) = xor arg1 arg2
evalOp arg1 (RotateL arg2) = rotateL arg1 arg2
evalOp arg1 (RotateR arg2) = rotateR arg1 arg2
evalOp arg1 (PBlock arg2) = evalPBlock arg1 arg2
evalOp arg1 (SBlock numOfParts sblock) = evalSBlock arg1 numOfParts sblock
evalOp arg1 (SBlocks numOfParts sblocks) = evalSBlocks arg1 numOfParts sblocks
-- evalOp arg1 (Custom function) = function arg1
evalOp _ _ = undefined

-- TODO: throw exception if length of pblock \= 32
-- Вычисляет P-блок
evalPBlock :: Word32 ->[Int] -> Word32
evalPBlock arg1 pblock = foldl (+) 0 (
                                zipWith (*) 
                                [if (testBit arg1 (pblock !! i)) then 1 else 0 | i <- [0..31]] 
                                [2^i | i <- [0..31]]) 

-- TODO: throw exception if length of pblock \= 2^i | i <- [0..31]
-- Разбивает слово на count частей
split :: Word32 -> Int -> [Word32]
split input count = 
                [shiftR input (size * i) .&. (2^size - 1) | i <- [0..(count-1)]]
                where size = quot 32 count 

-- Собирает слово из count частей                
aggregate :: [Word32] -> Int -> Word32
aggregate parts size = 
                foldl (+) 0
                (
                zipWith (*)
                [fromIntegral part | part <- parts]
                [(2^size)^i | i <- [0..(count-1)]]
                )
                where count = quot 32 size
                    
-- Вычисляет S-блок (с одним блоком замен)                
evalSBlock :: Word32 -> Int -> [Word32] -> Word32
evalSBlock input count sblock  =
                 aggregate [sblock !! (fromIntegral (splitted !! i)) | i <- [0..(count-1)]] size
                 where 
                         size = quot 32  count
                         splitted = split input count

-- Вычисляет S-блок (с несколькими блоками замен)        
evalSBlocks :: Word32 -> Int -> [[Word32]] -> Word32
evalSBlocks input count sblocks =
                aggregate [(sblocks !! i) !! (fromIntegral (splitted !! i)) | i <- [0..(count-1)]] size
                 where 
                         size = quot 32 count
                         splitted = split input count
                         
-- Основная функция модуля, выполняющая шифрование с помощью roundCount раундов сети фейстеля с заданной функцией f, 
-- заданным ключом и заданной функцией генерации раундовых ключей
encrypt :: Word64 -> Int -> FeistelFunction -> ([Word32] -> [Word64]) -> [Word32] -> Word64
encrypt plainText roundCount f keyAlgorithm key = feistel roundCount plainText key keyAlgorithm (wrapF (parseF f))   
                
                
testSBlocks :: Word32
testSBlocks = evalSBlocks 0x12345678 8 s
                        where 
                                s :: [[Word32]]                                
                                s = [[0..15] | i <- [0..7]]
