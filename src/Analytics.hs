module Analytics where
import Data.Bits
import Data.Word
import GostDSLBased

makeBitArray :: Word64 -> [Int]
makeBitArray arg = [if testBit arg i then 1 else 0 | i <- [0..31]]

compareBitArrays :: [Int] -> [Int] -> [Int]
compareBitArrays a b  = [x | x <- [if (a !! i) == (b !! i) then i else -1 | i <- [0..(len - 1)]], x >= 0]  
                                where len = length a

diffBitArrays :: [Int] -> [Int] -> [Int]
diffBitArrays a b  = [x | x <- [if (a !! i) /= (b !! i) then i else -1 | i <- [0..(len - 1)]], x >= 0]  
                                where len = length a
                                
checkBitDependency :: Word64 -> Int -> [Int]
checkBitDependency plainText bitNumber =
                                diffBitArrays (getEncryptionBitArray (setBit plainText bitNumber)) (getEncryptionBitArray (clearBit plainText bitNumber))
                                where 
                                        getEncryptionBitArray arg = (makeBitArray (gost arg [0..7]))

studyBitDependecy :: [Int] ->[(Int, Int)]
