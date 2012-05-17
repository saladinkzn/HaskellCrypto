module Gost where
import Numeric
import Data.Char
import Data.Bits
import Text.Show
import GHC.Word
import Feistel

main::IO()
main = undefined

-- Генерирует раундовые ключи по ключу
gost_round_keys :: [Word32] -> [Word64]
gost_round_keys key = [fromIntegral x | x <- key ++ key ++ key ++ reverse key]

-- Функция, f в схеме Фейстеля
gost_f :: Word32 -> Word64 -> Word32 
gost_f a k = rotateL (foldl (+) 0 (zipWith (*) (sblocks (split (a+(fromIntegral k)))) [2^x | x <- [0,4 .. 28]])) 11


-- Вычисляет шифр ГОСТ 28147-89
gost :: Word64 -> [Word32] -> Word64
gost input key = feistel 32 input key gost_round_keys gost_f



-- S-таблица
s :: [[Word32]]
s = [[4,10,9,2,13,8,0,14,6,11,1,12,7,15,5,3],
     [14,11,4,12,6,13,15,10,2,3,8,1,0,7,5,9],
     [5,8,1,13,10,3,4,2,14,15,12,7,6,0,9,11],
     [7,13,10,1,0,8,9,15,14,4,6,12,11,2,5,3],
     [6,12,7,1,5,15,13,8,4,10,9,14,0,3,11,2],
     [4,11,10,0,7,2,1,13,3,6,8,5,9,12,15,14],
     [13,11,4,1,3,15,5,9,0,10,14,7,6,8,2,12],
     [1,15,13,0,5,7,10,4,9,2,3,14,6,11,8,12]] 

-- Делит слово на 8 4х битных элемента
-- TODO: вынести кол-во элементов в параметр
split :: Word32 -> [Word32]
split input = [0xF .&. shiftR input j | j <- [0,4..28]]

-- Осуществляет подстановку
-- input - число
-- block - S-block
sblock :: Word32 -> [Word32] -> Word32
sblock input block = block !! fromIntegral input

-- Осуществляет подстановку
-- input - слово, разбитое на 8 4х битных элементов
-- s - константа, содеражащая 8 s-блоков 
-- TODO: выделить s в параметры
sblocks :: [Word32] -> [Word32]
sblocks input = [sblock (input !! i) (s !! i) | i <- [0..7]]                              
      


