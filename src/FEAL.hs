
module FEAL where
import Data.Word
import Data.Bits
import CryptoDSL

feal :: Word64 -> [Word32] -> Int -> Word64
feal plainText key n = post (fealInternal (pre plainText) key)
        where 
                keys = fealKeyAlgorithm key n
                pre :: Word64 -> Word64
                pre p = p + (foldl (+) 0 (zipWith (*) [keys !! (n+3), keys !! (n+2), keys !! (n+1), keys !! (n)] [(2^16)^i | i<-[0..3]]))
                post :: Word64 -> Word64
                post p = p + (foldl (+) 0 (zipWith (*) [keys !! (n+7), keys !! (n+6), keys !! (n+5), keys !! (n+4)] [(2^16)^i | i<-[0..3]]))
                 

fealInternal :: Word64 -> [Word32] -> Word64
fealInternal plainText key = encrypt plainText 4 fealF (\ k -> fealKeyAlgorithm k 4) key
        where 
                fealF :: FeistelFunction
                fealF = FeistelFunction [(CustomKey f)]

fealOR :: Word64 -> [Word32] -> Word64
fealOR plainText key = post (fealOneRoundIntegral (pre plainText) key)
        where 
                n = 4
                keys = fealKeyAlgorithm key n
                pre :: Word64 -> Word64
                pre p = p + (foldl (+) 0 (zipWith (*) [keys !! (n+3), keys !! (n+2), keys !! (n+1), keys !! (n)] [(2^16)^i | i<-[0..3]]))
                post :: Word64 -> Word64
                post p = p + (foldl (+) 0 (zipWith (*) [keys !! (n+7), keys !! (n+6), keys !! (n+5), keys !! (n+4)] [(2^16)^i | i<-[0..3]]))
                
fealOneRoundIntegral :: Word64 -> [Word32] -> Word64
fealOneRoundIntegral plainText key = encrypt plainText 2 fealF(\k -> fealKeyAlgorithm k 4) key
        where 
                fealF :: FeistelFunction
                fealF = FeistelFunction [(CustomKey f)]
                

-- TODO : Rewrite monkeycoding
f :: Word32 -> Word32 -> Word32
f a b = 
        foldl (+) 0 
        (zipWith (*) [fromIntegral x | x <- [f3,f2,f1,f0]] [256^i | i <- [0..3]])
        where 
                --f11 :: Word8
                --f11 = xor a1 b0
                --f21 :: Word8
                --f21 = xor a2 b1
                --f12 :: Word8
                --f12 = xor f11 f22
                --f22 :: Word8
                --f22 = xor f21 a3
                --
                --f13 :: Word32
                --f13 = fromIntegral (s1 f12 f22)
                --f23 :: Word32
                --f23 = fromIntegral (s0 f22 (fromIntegral f13))
                --f0 :: Word32
                --f0 = fromIntegral (s0 a0 (fromIntegral f13))
                --f3 :: Word32
                --f3 = fromIntegral (s1 a3 (fromIntegral f23))
                t1::Word8
                t1 = xor (xor a0 a1) b0
                t2::Word8
                t2 = xor (xor a2 a3) b1
                f1::Word8
                f1 = s1 t1 t2
                f0::Word8
                f0 = s0 a0 f1
                f2::Word8
                f2 = s0 t2 f1
                f3::Word8
                f3 = s1 f2 a3
                --
                a0 :: Word8
                a0 = fromIntegral (shiftR a 24 .&. 0xFF)
                a1 :: Word8
                a1 = fromIntegral (shiftR a 16 .&. 0xFF)
                a2 :: Word8
                a2 = fromIntegral (shiftR a 8 .&. 0xFF)
                a3 :: Word8
                a3 = fromIntegral (a .&. 0xFF)
                b0 :: Word8
                b0 = fromIntegral (shiftR b 8 .&. 0xFF)
                b1 :: Word8
                b1 = fromIntegral (b .&. 0xFF)
                
s0 :: Word8 -> Word8 -> Word8
s0 x1 x2 = rotateL t 2
                where t :: Word8
                      t = (x1 + x2) .&. 0xFF
s1 :: Word8 -> Word8 -> Word8
s1 x1 x2 = rotateL t 2 
                where t :: Word8
                      t = (x1 + x2 + 1) .&. 0xFF                              
                                                
fealKeyAlgorithm :: [Word32] -> Int -> [Word64]
fealKeyAlgorithm key n = [ t .&. 0xFFFF | t <- fealKeyAlgorithmWorker ((quot n 4) + 2) kl kr []]
                        where
                        kl = key !! 0
                        kr = key !! 1

fealKeyAlgorithmWorker :: Int -> Word32 -> Word32 -> [Word64] -> [Word64]
fealKeyAlgorithmWorker 0 _ _ result = result                        
fealKeyAlgorithmWorker n kl kr result = fealKeyAlgorithmWorker (n - 1) kr (fk kr ((fk kl kr) + kl)) (result ++ k01  ++ k23)  
                                        where 
                                        k01 :: [Word64]
                                        k01 = [shiftR fk1 16 .&. 0xFFFF, fk1 .&. 0xFFFF]
                                        k23 :: [Word64]
                                        k23 = [shiftR fk2 16 .&. 0xFFFF, fk2 .&. 0xFFFF]
                                        fk1 :: Word64
                                        fk1 = fromIntegral (fk kl kr)
                                        fk2 :: Word64
                                        fk2 = fromIntegral (fk kr (fromIntegral fk1 + kl))
                                        fk :: Word32 -> Word32 -> Word32
                                        fk x y = foldl (+) 0 (zipWith (*) [fromIntegral t | t <- [fk3, fk2, fk1, fk0]] [256^i | i <-[0..3]])
                                                        where   
                                                                t1 :: Word8
                                                                t1 = xor (_x !! 0) (_x !! 1)
                                                                t2 :: Word8
                                                                t2 = xor (_x !! 2) (_x !! 3)
                                                                
                                                                fk1 = s1 t1 (xor t2 (_y !! 0))
                                                                fk2 = s0 t2 (xor fk1 (_y !! 1))
                                                                fk0 = s0 (_x !! 0) (xor fk1 (_y !! 2))
                                                                fk3 = s1 (_x !! 3) (xor fk2 (_y !! 3))
                                                                _x :: [Word8]
                                                                _x = [fromIntegral t | t <- [shiftR x i .&. 0xFF | i <- [0,8,16,24]]]
                                                                _y :: [Word8]
                                                                _y = [fromIntegral t | t <- [shiftR y i .&. 0xFF | i <- [0,8,16,24]]]
                                      
                                