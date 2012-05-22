module DifferencialCryptoanalisys where
import FEALReformuled
import System.Random
import Data.Bits
import Data.List
import Data.Word


random seed = randoms (mkStdGen seed) 

-- Исследует функцию с помощью методики дифференциального криптоанализа (возвращает список полученных выходных дифференциалов для заданного входного)
surveyDiff :: Word32 -> (Word32 -> Word32) -> Int -> [Word32]
surveyDiff diff g' proximity =
        [xor (g' (trials !! i)) (g' (fromIntegral i))  | i<- randoms ]
                where  
                randoms = [x | x<- [0..(proximity-1)]]
                trials = [xor (fromIntegral i) diff  |  i <- randoms]

-- Исследует функцию с помощью методики дифференциального криптоанализа (возвращает вероятность получения какого-либо дифференциала для заданного входного)
analyzeSurvey diff g' proximity = [(x, (fromIntegral (count x survey)) / (fromIntegral (length survey)))  | x <- nub survey]
                        where 
                        survey = surveyDiff diff g' proximity

-- Возвращает максимальную вероятность появления какого-либо выходного дифференциала 
maxPossibility surveyAnalysis = maximum [b|(a,b) <- surveyAnalysis]

analyzeWeaknesses = maximum [maxPossibility x | x <- [analyzeSurvey i g 1000 | i <- [0..(2^6)-1]]] 

count :: Word32 -> [Word32]-> Int
count x list =  countHelper x list 0

countHelper :: Word32 -> [Word32] -> Int -> Int
countHelper _ [] count = count
countHelper x' (y:ys) count = if x'==y then countHelper x' ys (count+1) else countHelper x' ys count  
