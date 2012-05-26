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

         
        