module FEALReformuled where
import FEAL
import CryptoDSL
import Data.Bits
import Data.Word
import Data.List
import System.Random

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

    
bk :: Word64->  Int ->  Word32
bk _ (-2) = 0
bk k (-1) = fromIntegral ((shiftR k 32) .&. 0xFFFFFFFF)
bk k 0 = fromIntegral (k .&. 0xFFFFFFFF)
bk k i = if i < -2 then undefined else fk (bk k (fromIntegral (i-2))) (xor (bk k (fromIntegral (i-1))) (bk k (fromIntegral (i-3))))

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
              b = \i -> bk k i
              m1 = xor (b 3) (tetaR (b 1))
              n1 :: Word32
              n1 = xor (xor (b 3) (b 4)) (tetaL (b 1))
              m2 = xor (tetaL (b 1)) (tetaL (b 2))
              n2 = xor (tetaR (b 1)) (tetaR (b 2))
              m3 = xor (xor (b 5) (b 6)) (tetaR (b 1))
              n3 = xor (b 5) (tetaL (b 1)) 
               
-- TODO: rewrite to return [(Word8, Word8)]
z1z2 :: Word32 -> Word32 -> [(Word8, Word8)]
z1z2 a b = [(z1, z2) | z1<-[0..(fromIntegral 255)], z2<-[0..(fromIntegral 255)], (s1 (xor (xor z1 a0) a1) (xor (xor z2 a2) a3)) == (fromIntegral b1) ]
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
g1x a b = head [aggregate [fromIntegral (x3 _tmp), xor (fromIntegral (x3 _tmp)) (fromIntegral (z2 _tmp)), fromIntegral (xor (z1 _tmp) (x0 _tmp)), fromIntegral (x0 _tmp)] 8 | _tmp <- tmp] 
        where
        x0 _tmp = x0' _tmp
        x3 _tmp = x3' _tmp
        z1 _tmp = z1' _tmp
        z2 _tmp = z2' _tmp
        x0' :: (Word8,Word8,Word8,Word8) -> Word8
        x0' (a',_,_,_) = a'
        x3' :: (Word8,Word8,Word8,Word8) -> Word8
        x3' (_,b',_,_) = b'
        z1' :: (Word8,Word8,Word8,Word8) -> Word8
        z1' (_,_,c',_) = c'
        z2' :: (Word8,Word8,Word8,Word8) -> Word8
        z2' (_,_,_,d') = d'
        
        -- TODO: rewrite to consume  [(Word8, Word8)]
        tmp :: [(Word8,Word8,Word8,Word8)]
        tmp  = [(x0,x3, fst _z1z2, snd _z1z2) | x0 <-arr256, x3<-arr256, _z1z2 <- __z1z2,  condition x0 x3 a b _z1z2]
                where

                __z1z2 :: [(Word8,Word8)]
                __z1z2 = z1z2 a b
                
                
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
                 
convert :: [Word8] -> [Word32]
convert x = [fromIntegral x' | x' <- x]
g2x :: Word32 -> Word32 -> Word32 -> [Word32]
g2x a b d = nub  [aggregate (convert x') 8 | x' <- [[x3 _tmp,x2 (z1 _tmp) (x3 _tmp),x1 (z1 _tmp) (x0 _tmp),x0 _tmp] | _tmp <- tmp]]
                where
                x0 _tmp = x0' _tmp
                x3 _tmp = x3' _tmp
                x1 _z1 _x0 = xor _z1 _x0
                x2 _z2 _x3 = xor _z2 _x3
                z1 _tmp = z1' _tmp
                z2 _tmp = z2' _tmp
                x0' (_,_,c',_) = c'
                x3' (_,_,_,d') = d'
                z1' (a',_,_,_) = a'
                z2' (_,b',_,_) = b'
                g2z1z2' = g2z1z2 a b d
                tmp = [(_z1,_z2, _x0, _x3) | _x0 <- arr256, _x3 <- arr256, (_z1, _z2) <- g2z1z2', condition_g2x _z1 _z2 _x0 _x3 a b d]
condition_g2x z1' z2' x0' x3' a b d =
        --(xor alp1 bet1 == d1) && (xor alp2 bet2 == d2) &&
        ((xor (s1 alp2 (xor x3' a3)) (s1 bet2 (xor x3' b3))) == d0) &&
        ((xor (s0 alp1 (xor x0' a0)) (s0 bet1 (xor x0' b0))) == d3)
        where
        alp1 = s1 (xor (xor z1' a0) a1) (xor (xor z2' a2) a3)
        alp2 = s0 alp1 (xor (xor z2' a2) a3)
        bet1 = s1 (xor (xor z1' b0) b1) (xor (xor z2' b2) b3)
        bet2 = s0 bet1 (xor (xor z2' b2) b3)
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
                        
g2z1z2 a b d = [(z1,z2) | z1 <- arr256, z2 <- arr256, condition2 z1 z2 a b d]
condition2 z1 z2 a b d = (xor alp1 bet1 == d1) && (xor alp2 bet2 == d2)
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
genTestData4g2x a b c = xor (g (xor a b)) (g (xor a c))              

-- 4. Chosing the plain texts
left x = (shiftR x 32) .&. 0xFFFFFFFF
right x = x .&. 0xFFFFFFFF
q i p = xor (left (p!!i)) (right (p!!i))
d i c = xor (left (c!!i)) (right (c!!i))

rs :: [Int]
rs = randoms (mkStdGen 0)

p = [choose i | i <- [0..19]]
        where
        choose 0 = rs !! 0
        choose 12 = rs !! 1
        choose 14 = rs !! 2
        choose 16 = rs !! 3
        choose 17 = rs !! 4
        choose 18 = rs !! 5
        choose 19 = rs !! 6
-- 5.
w1 d0 d1 cl0 cl1 = g2x d0 d1 (xor (xor cl0 cl1) 02000000)
w2 d0 d2 cl0 cl2 = g2x d0 d2 (xor (xor cl0 cl2) 00000002)
w' d0 d1 d2 cl0 cl1 cl2 = intersect (w1 d0 d1 cl0 cl1) (w2 d0 d2 cl0 cl2)
w :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
w d0 d1 d2 d3 d4 cl0 cl1 cl2 cl3 cl4 =
        [_w | _w <- (w' d0 d1 d2 cl0 cl1 cl2), isTrue _w]
                where
                isTrue :: Word32 -> Bool
                isTrue w = xor (xor (xor (g (xor d0 w)) (g (xor d3 w))) cl0) cl3 == 01000000 &&
                           xor (xor (xor (g (xor d0 w)) (g (xor d3 w))) cl0) cl3 == 03000000 &&
                           xor (xor (xor (g (xor d0 w)) (g (xor d4 w))) cl0) cl4 == 00000001 &&
                           xor (xor (xor (g (xor d0 w)) (g (xor d4 w))) cl0) cl4 == 00000003
                          
v0 pl0 pl5 pl6 cl0 cl5 cl6 d0 d5 d6 w' =
        intersect (g2x pl0 pl5 r1) (g2x pl0 pl5 r2) 
        where 
        r1 = xor (xor cl0 cl5) (xor (g (xor d0 w')) (g (xor d5 w')))
        r2 = xor (xor cl0 cl6) (xor (g (xor d0 w')) (g (xor d6 w')))

u0 cli pli v0 di w = xor (xor cli (g (xor pli v0))) (g (xor di w))

testTriplets triplets cli pli di = [(w,v0,u0) | (w,v0,u0) <- triplets, testTriplet (w,v0,u0)]
                where
                testTriplet (w',v0',u0') =
                        and [testTripletHelper w' v0' u0' i | i <- [7..1]]
                        where 
                                testTripletHelper w'' v0'' u0'' i' =
                                        (xor (xor (xor (cli i') u0'') (g (xor (pli i') v0''))) (g (xor (di i') w''))) == 0
                                        
u12 u0 q0 q12 = xor (xor u0 q0) q12
u13 u0 q0 q12 = u12 u0 q0 q12
u14 u0 q0 q14 = xor (xor u0 q0) q14
u15 u0 q0 q14 = u14 u0 q0 q14
--
v12 pl12 d12 cl12 u12 w = g1x pl12 (xor (xor (g (xor d12 w)) cl12) u12)
v14 pl14 d14 cl14 u14 w = g1x pl14 (xor (xor (g (xor d14 w)) cl14) u14) 
--
resolve_n1 q0 q12 q14 v0 v12 v14 = intersect (g2x q0 q12 (xor v0 v12)) (g2x q0 q14 (xor v0 v14))
resolve_m2 y1'0 y1'17 y1'18 x1'0 x1'17 x1'18 d0 d17 d18 = [_m2 | _m2 <- intersect (g2x y1'0 y1'17 (xor (xor x1'0 x1'17) (xor d0 d17))) (g2x y1'0 y1'18 (xor (xor x1'0 x1'18) (xor d0 d18))), test _m2]
                                                        where 
                                                                test _m2 = 
                                                                        ((shiftR _m2 24) .&. 0xFF == 0) &&
                                                                        ((shiftR _m2 0) .&. 0xFF == 0)
resolve_m3 x1 y1 m2 d = xor (g (xor y1 m2)) (xor x1 d)
resolve_n3 u0 n1 qi = xor u0 (xor(qi n1))
resolve_m1 v0 n1 qi = xor (g (xor qi n1)) v0