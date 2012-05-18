module Feistel where
import Numeric
import Data.Char
import Data.Bits
import Text.Show
import GHC.Word

-- ������� ���� ����� ��������
-- i - ���-�� �����
-- a - ai
-- b - bi
-- k - ��������� �����
feistelLoop :: Int -> Int -> Word32 -> Word32 -> [Word64] -> (Word32->Word64->Word32) -> (Word64, Word64)
feistelLoop 0 _ a b _ _ = (fromIntegral a, fromIntegral b)
feistelLoop i totalRounds a b k f = feistelLoop (i-1) totalRounds (xor b (f a (k !! (totalRounds - i)))) a k f


-- input - ������� �������� �����
-- key - ����� �������� �� 4�������� ����� 
feistel :: Int -> Word64 ->  [Word32] -> ([Word32] -> [Word64]) -> (Word32 -> Word64 -> Word32) -> Word64
feistel rounds input key round_key_algorithm f =
        restore (feistelLoop rounds rounds (fromIntegral a) (fromIntegral b) (round_key_algorithm key) f)
        where
               a = 0xFFFFFFFF .&. input
               b = 0xFFFFFFFF .&. shiftR input 32
              
-- ��������� (a,b) ���, ����� A ���� �������� ������, � B - ��������
restore :: (Word64, Word64) -> Word64
restore (a,b) = (shiftL a 32) .|. (b)