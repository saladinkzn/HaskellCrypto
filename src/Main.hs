module Main where
import CryptoDSL
import Analytics
import Data.Word
import System.Random
import DifferencialCryptoanalisys


freqAnalysis :: (Word32->Word32->Word32) -> IO()
freqAnalysis cypher = do
        putStrLn "Enter preset bits: 0 for 0, 1 for 1, -1 for non-preset"
        str <- getLine
        let presetBits = read str
        putStrLn "Enter key"
        str <- getLine
        let key = read str
        putStrLn "Enter precision"
        str <- getLine
        let precision = read str
        let results = analyze3 presetBits (randoms (mkStdGen 0)) precision cypher key
        print (show results)  
   
diffAnalysis :: (Word32->Word32->Word32) -> IO()     
diffAnalysis cypher = do
        putStrLn "Enter key"
        str <- getLine
        let key = read str
        putStrLn "Enter deltaX"
        str <- getLine
        let dx = read str
        putStrLn "Enter precision"
        str <- getLine
        let precision = read str
        let f = \ x -> cypher x key
        let results = analyzeSurvey dx f precision
        print (show results) 
        
        

main :: IO ()
main = do
        putStrLn "Enter function for study"
        feistelString <- getLine
        -- Пример ввода: FeistelFunction [(SBlock 8 [4,10,9,2,13,8,0,14,6,11,1,12,7,15,5,3])] см. CryptoDSL
        let feistelFunction = read feistelString
        let lambda = parseF feistelFunction
        putStrLn "Enter 1. for frequency analysis or 2. for differencial analysis."
        typeString <- getLine
        let analysisType = read typeString
        if analysisType == 1 then freqAnalysis lambda else diffAnalysis lambda  
        
testParse :: IO()
testParse = do
        feistelString <- getLine
        let feistelFunction = read feistelString
        let lambda = parseF feistelFunction
        print (lambda 0 0)

         
-- Пример ввода для исследования ГОСТ :
-- FeistelFunction [(SumKey),(SBlocks 8 [[4,10,9,2,13,8,0,14,6,11,1,12,7,15,5,3],[14,11,4,12,6,13,15,10,2,3,8,1,0,7,5,9],[5,8,1,13,10,3,4,2,14,15,12,7,6,0,9,11],[7,13,10,1,0,8,9,15,14,4,6,12,11,2,5,3],[6,12,7,1,5,15,13,8,4,10,9,14,0,3,11,2],[4,11,10,0,7,2,1,13,3,6,8,5,9,12,15,14],[13,11,4,1,3,15,5,9,0,10,14,7,6,8,2,12],[1,15,13,0,5,7,10,4,9,2,3,14,6,11,8,12]]),(RotateL 11)]
-- 1
-- Фиксируем левую половину
-- [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
-- Вводим  ключ по выбору
-- Вводим кол-во значений, по которым будут вычисляться данные для анализа
-- 
-- Получаем на выходе список, содержащий вероятность появления бита на соответствующей позиции в выходном наборе  
        