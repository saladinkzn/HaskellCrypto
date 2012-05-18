module Analytics where
import Data.Bits
import Data.Word
import GostDSLBased
import System.Random

-- ��������� ����� � ������ 0 � 1, ��������������� �������� ������������� ��������� �����
makeBitArray :: Word64 -> [Int]
makeBitArray arg = [if testBit arg i then 1 else 0 | i <- [0..31]]

-- ���������� ������� � ���������� �������, ���� �� ������� � ��� ���������
compareBitArrays :: [Int] -> [Int] -> [Int]
compareBitArrays a b  = [x | x <- [if (a !! i) == (b !! i) then i else -1 | i <- [0..(len - 1)]], x >= 0]  
                                where len = length a

-- ���������� ������� � ���������� �������, ���� �� ������� � ��� ����������
diffBitArrays :: [Int] -> [Int] -> [Int]
diffBitArrays a b  = [x | x <- [if (a !! i) /= (b !! i) then i else -1 | i <- [0..(len - 1)]], x >= 0]  
                                where len = length a
                                
-- �� ��������� ��������� ������ ���������, ����� ���� �������������� ������ ������� �� bitNumber'��� ���� ��������� ������
checkBitDependency :: Word64 -> Int -> [Int]
checkBitDependency plainText bitNumber =
                                diffBitArrays (getEncryptionBitArray (setBit plainText bitNumber)) (getEncryptionBitArray (clearBit plainText bitNumber))
                                where 
                                        getEncryptionBitArray arg = (makeBitArray (gost arg [0..7]))

-- ��������� ������������� ���� � �������� ��������������� ����, ����� ������������ ��������� ����� � ���������������� ������
mask :: [Int] -> [Int] -> [Int] 
mask bits values = [if (bits !! i) /= -1 then (bits !! i) else (values !! i) | i <- [0..63]] 

-- ����������� ������ ��� � �����
aggregate :: [Int] -> Word64
aggregate values = foldl (+) 0 (zipWith (*) [fromIntegral value | value <- values] [2^i | i <- [0..63]])

-- ����������� ����� � ������ ���                  
splitWord64 :: Word64 -> [Int] 
splitWord64 value = [if testBit value i then 1 else 0 | i <- [0..63]]

-- ��������������� ������� ��� ���������� ���������� � ������� ��������� 1 � �������� ������� �����
increment :: ([Int], Int) -> [Int] -> ([Int], Int)
increment (current, total) addition = (zipWith (+) current addition, total + 1)

-- ����������� ����������� ��������� 1 � ������ ������� �������������� ������
-- TODO: ������� ������� ���������� � �������� 
-- presetBits - ������, �������� ������������� ����, ������: [-1,-1, ... , 1,1,1] - ��������, ��� 3 ������� ���� ����� �������� 1, � ��������� ����� 
-- �������� 0
-- TODO - �������� �� seed
-- randomNumbers - ����������� ������ ��������� 32-� ������ ����, ������������ ��� ��������� ��������� ���� ��� ����������
-- prescision - �������� (���-�� ����������� ��������� ���� � ���������������� ������)
analyze :: [Int] -> [Int] -> Int -> ([Int], Int)
analyze presetBits randomNumbers prescision = analyzeRounds prescision presetBits randomNumbers ((take 64 (cycle [0])), 0)
                        where analyzeRounds :: Int -> [Int] -> [Int] -> ([Int], Int)-> ([Int], Int)
                              analyzeRounds 0 _ _ state = state
                              analyzeRounds roundCount preset randoms state = 
                                        analyzeRounds (roundCount - 1) preset (drop 2 randoms) (getState preset randoms state)
                                        where getState :: [Int] -> [Int] -> ([Int], Int) ->  ([Int], Int)
                                              getState preset random state = 
                                                        increment state (evalState preset random)
                                                        where evalState :: [Int] -> [Int] -> [Int]
                                                              evalState preset random =
                                                                       splitWord64 ( gost (aggregate (mask preset (splitWord64 (unite random)))) [0..7])
                                                                       where unite :: [Int] -> Word64
                                                                             unite list = (shiftL (fromIntegral (list !! 0)) 32) .|. (fromIntegral (list !! 1))
                                                                             

testMask = mask ([-1|i<-[0..60]] ++ [1|i<-[61..63]]) (splitWord64 0xAAAAAAAAAAAAAAAA)
testAnalyze = analyze ([-1|i<-[0..60]] ++ [1|i<-[61..63]]) (randoms (mkStdGen 0)) (10^6)

-- ��������������� �������: ����������� ������� � �����������
-- ��������� ����������� ��������� 1 � ������ ������� �������������� �����
getProbabilities :: ([Int], Int) -> [Float]
getProbabilities (a, b) = [(fromIntegral  prob) / (fromIntegral b) |  prob <- a]
