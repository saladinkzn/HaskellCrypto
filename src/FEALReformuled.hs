module FEALReformuled where
import FEAL
import CryptoDSL
import Data.Bits
import Data.Word

g :: Word32 -> Word32
g a = aggregate [fromIntegral c | c<- [c3, c2, c1, c0]] 8
        where
                d1 :: Word8
                d1 = xor a0 a1
                d2 :: Word8
                d2 = xor a2 a3
                c1 :: Word8
                c1 = s1 d1 d2
                c2 :: Word8
                c2 = s0 d2 c1
                c0 :: Word8
                c0 = s0 a0 c1
                c3 :: Word8
                c3 = s1 a3 c2
                --
                a3 :: Word8
                a3 = fromIntegral (a .&. 0xFF)
                a2 :: Word8
                a2 = fromIntegral ((shiftR a 8) .&. 0xFF)
                a1 :: Word8  
                a1 = fromIntegral ((shiftR a 16) .&. 0xFF)
                a0 :: Word8
                a0 = fromIntegral ((shiftR a 24) .&. 0xFF) 

f :: Word32 -> Word32 -> Word32
f a b = g (aggregate ([fromIntegral x | x <- reverse [a0, xor a1 b0, xor a2 b1, a3]]) 8)
                where 
                a3 :: Word8
                a3 = fromIntegral (a .&. 0xFF)
                a2 :: Word8
                a2 = fromIntegral ((shiftR a 8) .&. 0xFF)
                a1 :: Word8  
                a1 = fromIntegral ((shiftR a 16) .&. 0xFF)
                a0 :: Word8
                a0 = fromIntegral ((shiftR a 24) .&. 0xFF) 
                b0 :: Word8
                b0 = fromIntegral (shiftR b 8 .&. 0xFF)
                b1 :: Word8
                b1 = fromIntegral (b .&. 0xFF)

tetaL :: Word32 -> Word32
tetaL a = aggregate [0, a1, a0, 0] 8
                where 
                a1 = (shiftR a 16) .&. 0xFF
                a0 = (shiftR a 24) .&. 0xFF 

tetaR :: Word32 -> Word32 
tetaR a = aggregate [0, a3, a2, 0] 8
                where 
                a3 = a .&. 0xFF
                a2 = (shiftR a 8) .&. 0xFF 





unite :: Word64 -> Word64 -> Word64
unite l r =
        (shiftL (l .&. 0xFFFFFFFF) 32) + (r .&. 0xFFFFFFFF)

feal4 :: Word64 -> Word64 -> Word64
feal4 p k = unite (fromIntegral cL) (fromIntegral cR)
        where cR = xor (xor x2 m3) cL
              cL = xor y2 n3
              y2 = xor y1 (g (xor x2 n2))
              x2 = xor x1 (g (xor y1 m2))
              y1 = xor y0 (g x1)
              x1 = xor x0 (g y0)
              y0 = xor (xor pL pR) n1
              x0 = xor pL m1
              pL :: Word32
              pL = fromIntegral ((shiftR p 32) .&. 0xFFFFFFFF)
              pR :: Word32
              pR = fromIntegral (p .&. 0xFFFFFFFF)
              --
              b :: Int -> Word32
              b (-2) = 0
              b (-1) = fromIntegral ((shiftR k 32) .&. 0xFFFFFFFF)
              b 0 = fromIntegral (k .&. 0xFFFFFFFF)
              b i = if i < -2 then undefined else fk (b (fromIntegral (i-2))) (xor (b (fromIntegral (i-1))) (b (fromIntegral (i-3))))
              --
              m1 = xor (b 3) (tetaR (b 1))
              n1 :: Word32
              n1 = xor (xor (b 3) (b 4)) (tetaL (b 1))
              m2 = xor (tetaL (b 1)) (tetaL (b 2))
              n2 = xor (tetaR (b 1)) (tetaR (b 2))
              m3 = xor (xor (b 5) (b 6)) (tetaR (b 1))
              n3 = xor (b 5) (tetaL (b 1)) 
               
-- TODO: rewrite to return [(Word8, Word8)]
z1z2 :: Word32 -> Word32 -> (Word8, Word8)
z1z2 a b = head [(z1, z2) | z1<-[0..(fromIntegral 255)], z2<-[0..(fromIntegral 255)], (s1 (xor (xor z1 a0) a1) (xor (xor z2 a2) a3)) == (fromIntegral b1) ]
                        where 
                                 a3 :: Word8
                                 a3 = fromIntegral (a .&. 0xFF)
                                 a2 :: Word8
                                 a2 = fromIntegral ((shiftR a 8) .&. 0xFF)
                                 a1 :: Word8  
                                 a1 = fromIntegral ((shiftR a 16) .&. 0xFF)
                                 a0 :: Word8
                                 a0 = fromIntegral ((shiftR a 24) .&. 0xFF)
                                 b1 :: Word8 
                                 b1 = fromIntegral ((shiftR b 16) .&. 0xFF)

  
-- решает уравнение g (xor x a) = b
g1x a b = aggregate [fromIntegral x3 , xor (fromIntegral x3) (fromIntegral z2), fromIntegral (xor z1 x0), fromIntegral (x0)] 8
        where
        x0 = x0' tmp
        x3 = x3' tmp
        z1 = z1' tmp
        z2 = z2' tmp
        x0' :: (Word8,Word8,Word8,Word8) -> Word8
        x0' (a,b,c,d) = a
        x3' :: (Word8,Word8,Word8,Word8) -> Word8
        x3' (a,b,c,d) = b
        z1' :: (Word8,Word8,Word8,Word8) -> Word8
        z1' (a,b,c,d) = c
        z2' :: (Word8,Word8,Word8,Word8) -> Word8
        z2' (a,b,c,d) = d
        
        -- TODO: rewrite to consume  [(Word8, Word8)]
        tmp :: (Word8,Word8,Word8,Word8)
        tmp  = head [(x0,x3, fst _z1z2, snd _z1z2) | x0 <-arr256, x3<-arr256, condition x0 x3 a b _z1z2]
                where

                _z1z2 :: (Word8,Word8)
                _z1z2 = z1z2 a b
                
                
arr256 :: [Word8]
arr256 = [0..(fromIntegral 255)]
condition :: Word8 -> Word8 ->  Word32 -> Word32 -> (Word8, Word8)  ->  Bool
condition x0 x3 a b _z1z2 =
        (s0 b1 (xor (xor (snd _z1z2) a2) a3) == b2) &&
        (s0 b1 (xor x0 a0) == b0) &&
        (s1 b2 (xor x3 a3) == b3) 
                where 
                 a3 :: Word8
                 a3 = fromIntegral (a .&. 0xFF)
                 a2 :: Word8
                 a2 = fromIntegral ((shiftR a 8) .&. 0xFF)
                 a1 :: Word8  
                 a1 = fromIntegral ((shiftR a 16) .&. 0xFF)
                 a0 :: Word8
                 a0 = fromIntegral ((shiftR a 24) .&. 0xFF)
                 --
                 b3 = fromIntegral (b .&. 0xFF)
                 b2 :: Word8
                 b2 = fromIntegral ((shiftR b 8) .&. 0xFF)
                 b1 :: Word8  
                 b1 = fromIntegral ((shiftR b 16) .&. 0xFF)
                 b0 :: Word8
                 b0 = fromIntegral ((shiftR b 24) .&. 0xFF)
                 
g2x a b d = aggregate [fromIntegral x | x <- [x3,x2,x1,x0]] 8
                where
                x0 = x0' tmp
                x3 = x3' tmp
                x1 = xor z1 x0
                x2 = xor z2 x3
                z1 = z1' tmp
                z2 = z2' tmp
                x0' (a,b,c,d) = c
                x3' (a,b,c,d) = d
                z1' (a,b,c,d) = a
                z2' (a,b,c,d) = b 
                g2z1z2 = head [(z1,z2) | z1 <- arr256, z2 <- arr256, condition2 z1 z2]
                        where 
                        condition2 z1 z2 = (xor alp1 bet1 == d1) && (xor alp2 bet2 == d2)
                                where
                                alp1 = s1 (xor (xor z1 a0) a1) (xor (xor z2 a2) a3)
                                alp2 = s0 alp1 (xor (xor z2 a2) a3)
                                bet1 = s1 (xor (xor z1 b0) b1) (xor (xor z2 b2) b3)
                                bet2 = s0 bet1 (xor (xor z2 b2) b3)
                                --
                                a3 :: Word8
                                a3 = fromIntegral (a .&. 0xFF)
                                a2 :: Word8
                                a2 = fromIntegral ((shiftR a 8) .&. 0xFF)
                                a1 :: Word8  
                                a1 = fromIntegral ((shiftR a 16) .&. 0xFF)
                                a0 :: Word8
                                a0 = fromIntegral ((shiftR a 24) .&. 0xFF)
                                --
                                b3 :: Word8
                                b3 = fromIntegral (b .&. 0xFF)
                                b2 :: Word8
                                b2 = fromIntegral ((shiftR b 8) .&. 0xFF)
                                b1 :: Word8  
                                b1 = fromIntegral ((shiftR b 16) .&. 0xFF)
                                b0 :: Word8
                                b0 = fromIntegral ((shiftR b 24) .&. 0xFF)
                                --
                                d3 :: Word8
                                d3 = fromIntegral (d .&. 0xFF)
                                d2 :: Word8
                                d2 = fromIntegral ((shiftR d 8) .&. 0xFF)
                                d1 :: Word8  
                                d1 = fromIntegral ((shiftR d 16) .&. 0xFF)
                                d0 :: Word8
                                d0 = fromIntegral ((shiftR d 24) .&. 0xFF) 
                        
                tmp = head [(fst g2z1z2,snd g2z1z2,x0,x3) | x0 <- arr256, x3 <- arr256, condition z1 z2 x0 x3]
                condition z1 z2 x0 x3 =
                        --(xor alp1 bet1 == d1) && (xor alp2 bet2 == d2) &&
                        (xor (s1 alp2 (xor x3 a3)) (s1 bet2 (xor x3 b3)) == d0) &&
                        (xor (s0 alp1 (xor x0 a0)) (s0 bet1 (xor x0 b0)) == d3)
                        where
                        alp1 = s1 (xor (xor z1 a0) a1) (xor (xor z2 a2) a3)
                        alp2 = s0 alp1 (xor (xor z2 a2) a3)
                        bet1 = s1 (xor (xor z1 b0) b1) (xor (xor z2 b2) b3)
                        bet2 = s0 bet1 (xor (xor z2 b2) b3)
                        --
                        a3 :: Word8
                        a3 = fromIntegral (a .&. 0xFF)
                        a2 :: Word8
                        a2 = fromIntegral ((shiftR a 8) .&. 0xFF)
                        a1 :: Word8  
                        a1 = fromIntegral ((shiftR a 16) .&. 0xFF)
                        a0 :: Word8
                        a0 = fromIntegral ((shiftR a 24) .&. 0xFF)
                        --
                        b3 :: Word8
                        b3 = fromIntegral (b .&. 0xFF)
                        b2 :: Word8
                        b2 = fromIntegral ((shiftR b 8) .&. 0xFF)
                        b1 :: Word8  
                        b1 = fromIntegral ((shiftR b 16) .&. 0xFF)
                        b0 :: Word8
                        b0 = fromIntegral ((shiftR b 24) .&. 0xFF)
                        --
                        d3 :: Word8
                        d3 = fromIntegral (d .&. 0xFF)
                        d2 :: Word8
                        d2 = fromIntegral ((shiftR d 8) .&. 0xFF)
                        d1 :: Word8  
                        d1 = fromIntegral ((shiftR d 16) .&. 0xFF)
                        d0 :: Word8
                        d0 = fromIntegral ((shiftR d 24) .&. 0xFF)                         