module GostDSLBased where
import CryptoDSL
import Data.Word


-- ��������� ���� ���� 28147-89, �������� � ������� ������� f ����� �������� � ������� ��������� ���������� �����
-- input - Word64 - �������� �����
-- key - [Word32] - 256-������ ����, �������������� � ���� 8 32�� ������ ���� (�� ������� ��� � �������) 
gost :: Word64 -> [Word32] -> Word64
gost input key =
        encrypt input 32 gostF gostKeyAlgorithm key
        where 
                gostF :: FeistelFunction
                gostF = FeistelFunction [(SumKey),(SBlocks 8 s),(RotateL 11)]
                                where s :: [[Word32]]
                                      s =  [[4,10,9,2,13,8,0,14,6,11,1,12,7,15,5,3],
                                            [14,11,4,12,6,13,15,10,2,3,8,1,0,7,5,9],
                                            [5,8,1,13,10,3,4,2,14,15,12,7,6,0,9,11],
                                            [7,13,10,1,0,8,9,15,14,4,6,12,11,2,5,3],
                                            [6,12,7,1,5,15,13,8,4,10,9,14,0,3,11,2],
                                            [4,11,10,0,7,2,1,13,3,6,8,5,9,12,15,14],
                                            [13,11,4,1,3,15,5,9,0,10,14,7,6,8,2,12],
                                            [1,15,13,0,5,7,10,4,9,2,3,14,6,11,8,12]] 
                        
                gostKeyAlgorithm :: [Word32] -> [Word64]
                gostKeyAlgorithm key = [fromIntegral x | x <- key ++ key ++ key ++ reverse key] 
                
                
