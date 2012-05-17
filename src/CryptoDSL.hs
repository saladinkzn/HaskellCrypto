module CryptoDSL where
import Data.Word
import Data.Bits
import Feistel

data Operator = Xor Word32 |
                RotateL Int |
                RotateR Int |
                PBlock [Int] |
                SBlock Int [Word32] |
                SBlocks Int [[Word32]] |
                Custom (Word32 -> Word32) |
                CustomKey (Word32 -> Word32 -> Word32) |
                XorKey |
                SumKey

data FeistelFunction = FeistelFunction [Operator]  

wrapF :: (Word32 -> Word32 -> Word32) -> (Word32 -> Word64 -> Word32)
wrapF function = \ a k -> function a (fromIntegral k)

parseF :: FeistelFunction -> (Word32 -> Word32 -> Word32)
parseF (FeistelFunction operators) =
        foldl superposition same operators
        where
                superposition :: (Word32 -> Word32 -> Word32) -> Operator -> (Word32->Word32->Word32)
                superposition f XorKey = \ a k -> xor (f a k) k
                superposition f (CustomKey g) = \ a k -> g (f a k) k
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
evalOp arg1 (Custom function) = function arg1
evalOp _ _ = undefined

-- TODO: throw exception if length of pblock \= 32
evalPBlock :: Word32 ->[Int] -> Word32
evalPBlock arg1 pblock = foldl (+) 0 (
                                zipWith (*) 
                                [if (testBit arg1 (pblock !! i)) then 1 else 0 | i <- [0..31]] 
                                [2^i | i <- [0..31]]) 

-- TODO: throw exception if length of pblock \= 2^i | i <- [0..31]
split :: Word32 -> Int -> [Word32]
split input count = 
                [shiftR input (size * i) .&. (2^size - 1) | i <- [0..(count-1)]]
                where size = quot 32 count 
                
aggregate :: [Word32] -> Int -> Word32
aggregate parts size = 
                foldl (+) 0
                (
                zipWith (*)
                [fromIntegral part | part <- parts]
                [(2^size)^i | i <- [0..(count-1)]]
                )
                where count = quot 32 size
                                
evalSBlock :: Word32 -> Int -> [Word32] -> Word32
evalSBlock input count sblock  =
                 aggregate [sblock !! (fromIntegral (splitted !! i)) | i <- [0..(count-1)]] size
                 where 
                         size = quot 32  count
                         splitted = split input count
        
evalSBlocks :: Word32 -> Int -> [[Word32]] -> Word32
evalSBlocks input count sblocks =
                aggregate [(sblocks !! i) !! (fromIntegral (splitted !! i)) | i <- [0..(count-1)]] size
                 where 
                         size = quot 32 count
                         splitted = split input count
                         

encrypt :: Word64 -> Int -> FeistelFunction -> ([Word32] -> [Word64]) -> [Word32] -> Word64
encrypt plainText roundCount f keyAlgorithm key = feistel roundCount plainText key keyAlgorithm (wrapF (parseF f))   
                
                
testSBlocks :: Word32
testSBlocks = evalSBlocks 0x12345678 8 s
                        where 
                                s :: [[Word32]]                                
                                s = [[0..15] | i <- [0..7]]
