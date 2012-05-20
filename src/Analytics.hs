module Analytics where
import Data.Bits
import Data.Word
import GostDSLBased
import FEAL
import System.Random

-- Переводит слово в массив 0 и 1, соответствующих битовому представлению заданного слова
makeBitArray :: Word64 -> [Int]
makeBitArray arg = [if testBit arg i then 1 else 0 | i <- [0..31]]

-- Сравнивает массивы и возвращает индексы, биты на которых у них совпадают
compareBitArrays :: [Int] -> [Int] -> [Int]
compareBitArrays a b  = [x | x <- [if (a !! i) == (b !! i) then i else -1 | i <- [0..(len - 1)]], x >= 0]  
                                where len = length a

-- Сравнивает массивы и возвращает индексы, биты на которых у них отличаются
diffBitArrays :: [Int] -> [Int] -> [Int]
diffBitArrays a b  = [x | x <- [if (a !! i) /= (b !! i) then i else -1 | i <- [0..(len - 1)]], x >= 0]  
                                where len = length a
                                
-- По заданному открытому тексту вычисляет, какие биты зашифрованного текста зависят от bitNumber'ого бита открытого текста
checkBitDependency :: Word64 -> Int -> (Word64 -> [Word32] -> Word64) -> [Int]
checkBitDependency plainText bitNumber cypher =
                                diffBitArrays (getEncryptionBitArray (setBit plainText bitNumber)) (getEncryptionBitArray (clearBit plainText bitNumber))
                                where 
                                        getEncryptionBitArray arg = (makeBitArray (cypher arg [0..7]))

-- Совмещает фиксированные биты и случайно сгенерированные биты, чтобы сформировать случайное слово с зафиксированными битами
mask :: [Int] -> [Int] -> [Int] 
mask bits values = [if (bits !! i) /= -1 then (bits !! i) else (values !! i) | i <- [0..63]] 

-- Преобразует массив бит в число
aggregate :: [Int] -> Word64
aggregate values = foldl (+) 0 (zipWith (*) [fromIntegral value | value <- values] [2^i | i <- [0..63]])

-- Преобразует число в массив бит                  
splitWord64 :: Word64 -> [Int] 
splitWord64 value = [if testBit value i then 1 else 0 | i <- [0..63]]

-- Вспомогательная функция для накопления информации о частоте появления 1 в заданной позиции слова
increment :: ([Int], Int) -> [Int] -> ([Int], Int)
increment (current, total) addition = (zipWith (+) current addition, total + 1)

-- Вспомогательная функция для накопления информации о кол-во единичных бит в слове
increment2 :: ([Int], Int) -> [Int] -> ([Int], Int)
increment2 (current, total) addition = (zipWith (+) current (addition2 addition), total + 1)
 
addition2 :: [Int] -> [Int]
addition2 x  = [0|i<-[0..pos-1]]++[1]++[0|i<-[pos+1..64]]
        where 
        pos = foldl (+) 0 x 

-- Анализирует вероятность появления 1 в каждой позиции зашифрованного текста
-- TODO: вынести функцию шифрования в параметр 
-- presetBits - массив, задающий фиксированные виды, пример: [-1,-1, ... , 1,1,1] - означает, что 3 старших бита имеют значение 1, а остальные имеют 
-- значение 0
-- TODO - заменить на seed
-- randomNumbers - бесконечный список случайных 32-х битных слов, используется для генерации случайных слов для шифрования
-- prescision - точность (кол-во тестируемых случайных слов с зафиксированными битами)
analyze :: [Int] -> [Int] -> Int -> (Word64 -> [Word32] -> Word64) -> ([Int], Int)
analyze presetBits randomNumbers prescision cypher = analyzeRounds prescision presetBits randomNumbers ((take 64 (cycle [0])), 0)
                        where analyzeRounds :: Int -> [Int] -> [Int] -> ([Int], Int)-> ([Int], Int)
                              analyzeRounds 0 _ _ state = state
                              analyzeRounds roundCount preset randoms state = 
                                        analyzeRounds (roundCount - 1) preset (drop 2 randoms) (getState preset randoms state)
                                        where getState :: [Int] -> [Int] -> ([Int], Int) ->  ([Int], Int)
                                              getState preset random state = 
                                                        increment state (evalState preset random)
                                                        where evalState :: [Int] -> [Int] -> [Int]
                                                              evalState preset random =
                                                                       splitWord64 ( cypher (aggregate (mask preset (splitWord64 (unite random)))) [0..7])
                                                                       
-- randomNumbers - бесконечный список случайных 32-х битных слов, используется для генерации случайных слов для шифрования
-- prescision - точность (кол-во тестируемых случайных слов с зафиксированными битами)
analyze2 :: [Int] -> [Int] -> Int -> (Word64 -> [Word32] -> Word64) -> ([Int], Int)
analyze2 presetBits randomNumbers prescision cypher = analyzeRounds prescision presetBits randomNumbers ((take 65 (cycle [0])), 0)
                        where analyzeRounds :: Int -> [Int] -> [Int] -> ([Int], Int)-> ([Int], Int)
                              analyzeRounds 0 _ _ state = state
                              analyzeRounds roundCount preset randoms state = 
                                        analyzeRounds (roundCount - 1) preset (drop 2 randoms) (getState preset randoms state)
                                        where getState :: [Int] -> [Int] -> ([Int], Int) ->  ([Int], Int)
                                              getState preset random state = 
                                                        increment2 state (evalState preset random)
                                                        where evalState :: [Int] -> [Int] -> [Int]
                                                              evalState preset random =
                                                                       splitWord64 ( cypher (aggregate (mask preset (splitWord64 (unite random)))) [0..7])
                                                                       --where unite :: [Int] -> Word64
                                                                             --unite list = (shiftL (fromIntegral (list !! 0)) 32) .|. (fromIntegral (list !! 1))
unite :: [Int] -> Word64
unite list = (shiftL (fromIntegral (list !! 0)) 32) + (fromIntegral (list !! 1))                                                                     

testMask = mask ([-1|i<-[0..60]] ++ [1|i<-[61..63]]) (splitWord64 0xAAAAAAAAAAAAAAAA)
testAnalyze = analyze ([-1|i<-[0..60]] ++ [1|i<-[61..63]]) (randoms (mkStdGen 0)) (10^4) gost
genMask startPos endPos = ([0|i<-[0..(startPos - 1)]] ++ [-1|i<-[startPos..endPos]] ++ [1|i<-[(endPos + 1)..63]])
testAnalyzeGost startPos endPos  = analyze (genMask startPos endPos)   (randoms (mkStdGen 0)) (10^4) gost
testAnalyze2 seed = analyze ([0|i<-[0..25]] ++ [-1] ++ [1|i<-[27..63]]) (randoms (mkStdGen seed)) (10^4) (\ p k -> feal p k 4)
testAnalyze3 seed = analyze (genMask 31 32) (randoms (mkStdGen seed)) (10^4) (\ p _ -> p )
testAnalyze5 seed = analyze ([-1|i<-[0..63]]) (randoms (mkStdGen seed)) (10^4) (\ p _ -> p )
testAnalyze4 seed bit = analyze ([0|i<-[0..bit-1]] ++ [-1] ++ [1|i<-[(bit+1)..63]]) (randoms (mkStdGen seed)) (10^3) (\ p k -> feal p k 4)
testAnalyze6 seed startBit stopBit = analyze (genMask startBit stopBit) (randoms (mkStdGen seed)) (10^4) (\ p k -> feal p k 4)
testAnalyze61 seed startBit stopBit = analyze (genMask startBit stopBit) (randoms (mkStdGen seed)) (10^4) (\ p _ -> p )

testAnalyze2_61 seed startBit stopBit = analyze2 (genMask startBit stopBit) (randoms (mkStdGen seed)) (10^4) (\ p _ -> p )
testAnalyze2_6 seed startBit stopBit = analyze2 (genMask startBit stopBit) (randoms (mkStdGen seed)) (10^4) feal4
--
feal4 p k = feal p k 4

testAnalyze7 seed = analyze ([-1|i<-[0..31]]++[0|i<-[32..63]]) (randoms (mkStdGen 0)) (10^4) feal4
testAnalyze8 seed = analyze ([0|i<-[0..31]]++[-1|i<-[32..63]]) (randoms (mkStdGen 0)) (10^4) feal4
--
testAnalyze9 seed = analyze ([-1|i<-[0..31]]++[0|i<-[32..63]]) (randoms (mkStdGen 0)) (10^4) fealOR 
testDep bit = checkBitDependency 0xAAAAAAAAAAAAAAAA bit (\ p k -> feal p k 4)

-- Вспомогательная функция: преобразует частоты в вертояности
-- Вычисляет вероятность появления 1 в каждой позиции зашифрованного теста
getProbabilities :: ([Int], Int) -> [Float]
getProbabilities (a, b) = [(fromIntegral  prob) / (fromIntegral b) |  prob <- a]
