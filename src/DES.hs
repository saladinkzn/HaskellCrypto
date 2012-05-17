
module DES where
import GHC.Word
import Data.Word 
import Feistel
import Data.Bits


enlarge :: Word32 -> Word64
enlarge a = foldl (+) 0 (zipWith (*) [if testBit a (e !! i) then 1 else 0 | i <- [0..47]] [2^i | i <-[0..47]])
        where 
        e :: [Int]
        e =    [x-1 | x <- [32, 1, 2, 3, 4, 5,
                 4,  5, 6, 7, 8, 9,
                 8,  9,10,11,12,13,
                 12,13,14,15,16,17,
                 16,17,18,19,20,21,
                 20,21,22,23,24,25,
                 24,25,26,27,28,29,
                 28,29,30,31,32,1]]

uniteKey :: [Word32] -> Word64
uniteKey parts = fromIntegral (parts !! 0) + 2^32 * fromIntegral (parts !! 1)

split8 :: Word64 -> [Word64]
split8 key = [fromIntegral (0xFF .&. shiftR key (8*i)) | i <-[0..7]]

add8::[Word64] -> Word64
add8 keys = foldl (+) 0 (zipWith (*) [if testBit (sum7 key) 0 then clearBit key 7 else setBit key 7 | key <- keys] [256^i | i <-[0..7]])

                where
                sum7 :: Word64 -> Word64 
                sum7 input = foldl (+) 0 [if testBit input i then 1 else 0 | i <- [0..6]]
        
shuffle :: Word64 -> Word64
shuffle input = foldl (+) 0 (zipWith (*) [if testBit input (64 - (pblock !! i)) then 1 else 0 | i <- reverse [0..55]] [2^i | i <- [0..63]])
        where pblock = [x|x<-[        57,49,41,33,25,17,9,
                                        1,58,50,42,34,26,18,
                                        10,2,59,51,43,35,27,
                                        19,11,3,60,52,44,36,
                                        63,55,47,39,31,23,15,
                                        7,62,54,46,38,30,22,
                                        14,6,61,53,45,37,29,
                                        21,13,5,28,20,12,4]]
rotateUtil :: Word64 -> Int -> Word64                        
rotateUtil x i = (shiftL ((2^(28-i) - 1) .&. x) i) .|. (shiftR ((shiftL (2^i-1) (28-i)) .&. x) (28-i))  -- (shiftR ((2^i-1) .&. x) (28 - i)) .|. ((shiftR x i) .&. (2^(28-i)-1))

generateRound :: Word64 -> [Word64]
generateRound input = 
                [(shiftL (rotateUtil c (rotateBlock !! i)) 28) .|. (rotateUtil d (rotateBlock !! i)) | i <- [0..15]]
                        where 
                                c = 0xFFFFFFF .&. (shiftR input 28)
                                d = 0xFFFFFFF .&. input 
                                --rotateUtil x i = (shiftR ((2^i-1) .&. x) (28 - i)) .|. ((shiftR x i) .&. (2^(28-i)-1))
                                --rotateBlock = [1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]
                                rotateBlock = [1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28]

selectOne :: Word64 -> Word64
selectOne x = foldl (+) 0 (zipWith (*) [if testBit x (56 - (selectBlock !! i)) then 1 else 0 | i <- reverse [0..47]] [2^i | i <- [0..47]])
                        where selectBlock = [x | x<- [
                               14,17,11,24,1,5,
                               3,28,15,6,21,10,
                               23,19,12,4,26,8,
                               16,7,27,20,13,2,
                               41,52,31,37,47,55,
                               30,40,51,45,33,48,
                               44,49,39,56,34,53,
                               46,42,50,36,29,32]]

select :: [Word64] -> [Word64]
select keys = [selectOne key | key <- keys]

des_round_key :: [Word32] -> [Word64]
des_round_key key = select (generateRound (shuffle (add8 (split8 (uniteKey key)))))
        where 
                
        
des_sblocks :: Word64 -> Word32
des_sblocks a = foldl (+) 0 (zipWith (*) [((s !! i) !! ((splitX a) !! i)) !! ((splitY a) !! i) | i <-[0..7]] [16^i | i <-[0..7]])
                       where 
                       s :: [[[Word32]]]
                       s =     [
                                [
                                 [14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7],
                                 [0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8],
                                 [4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0],
                                 [15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13]
                                ],
                                [
                                 [15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10],
                                 [3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5],
                                 [0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15],
                                 [13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9]
                                ],
                                [
                                 [10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8],
                                 [13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1],
                                 [13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7],
                                 [1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12]
                                ],
                                [
                                 [7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15],
                                 [13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9],
                                 [10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4],
                                 [3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14]
                                ],
                                [
                                 [2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9],
                                 [14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6],
                                 [4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14],
                                 [11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3]
                                ],
                                [
                                 [12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11],
                                 [10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8],
                                 [9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6],
                                 [4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13]
                                ],
                                [
                                 [4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1],
                                 [13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6],
                                 [1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2],
                                 [6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12]
                                ],
                                [
                                 [13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7],
                                 [1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2],
                                 [7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8],
                                 [2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11]
                                ]
                                ]
   
splitX :: Word64 -> [Int]
splitX a = [fromIntegral ((2 * (0x1 .&. shiftR x 5)) + (0x1 .&. x)) | x <- [0x3F .&. shiftR a (6*i) | i <- [0..7]]]
splitY :: Word64 -> [Int]
splitY a = [fromIntegral (0xF .&. (shiftR x 1)) | x <- [0x3F .&. shiftR a (6*i) | i <- [0..7]]]                        

des_f :: Word32 -> Word64 -> Word32
des_f a k = des_shift (des_sblocks (xor (enlarge a) k))
        where 
                des_shift :: Word32 -> Word32
                des_shift a = 
                        foldl (+) 0 (zipWith (*) [if testBit a x then 1 else 0 | x <- pblock] [2^i | i <-[0..31]])
                                where 
                                pblock :: [Int]
                                pblock = [x-1 | x <- [ 16,7,20,21,29,12,28,17,1,15,23,26,5,18,31,10,2,8,24,14,32,27,3,9,19,13,30,6,22,11,4,25]]
                               

preShift :: Word64 -> Word64
preShift input = foldl (+) 0 (zipWith(*) [if testBit input x then 1 else 0 | x <- pblock] [2^i | i <-[0..63]])
        where 
        pblock = [x-1 | x <-[58,50,42,34,26,18,10,2,60,52,44,36,28,20,12,4,
                 62,54,46,38,30,22,14,6,64,56,48,40,32,24,16,8,
                 57,49,41,33,25,17,9,1,59,51,43,35,27,19,11,3,
                 61,53,45,37,29,21,13,5,63,55,47,39,31,23,15,7]]

postShift :: Word64 -> Word64
postShift input = foldl (+) 0 (zipWith(*) [if testBit input x then 1 else 0 | x <- pblock] [2^i | i <-[0..63]])
        where 
        pblock = [x-1 | x <- [40,8,48,16,56,24,64,32,39,7,47,15,55,23,63,31,
                  38,6,46,14,54,22,62,30,37,5,45,13,53,21,61,29,
                  36,4,44,12,52,20,60,28,35,3,43,11,51,19,59,27,
                  34,2,42,10,50,18,58,26,33,1,41,9,49,17,57,25]]

des :: Word64 -> Word64 -> Word64
des input key = 
        postShift (feistel 16 (preShift input) splitted_key des_round_key des_f)
        where 
        splitted_key :: [Word32]
        splitted_key = [fromIntegral x | x <- [a, b]]
                where  
                        a = 0xFFFFFFFF .&. key
                        b = 0xFFFFFFFF .&. shiftR key 32 
                        
test_des = (des 0 0x0706050403020100) == 5192420752798849967
test_shuffle = shuffle 0x133457799BBCDFF1 == 67779029043144591
temp_shuffle = shuffle 0x133457799BBCDFF1
temp_key = (select (generateRound  temp_shuffle)) !! 0
test_key = temp_key == 29699430183026

c1d1 = generateRound temp_shuffle !! 0