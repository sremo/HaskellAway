import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Vector.Unboxed as U
--import qualified Data.ByteString.Lex.Lazy.Double as W
import Data.List

import Numeric.LinearAlgebra
--import Numeric.LinearAlgebra.LAPACK

main = do  
    contents <- L.readFile "quotes.txt"  
    --let vec = L.map W.readDouble contents --(words contents)
    --let vec = L.map L.read (modWords contents)::Double
    let vec = map readPriceMod (C.words contents)
    --let vec =  W.readDouble contents--(words contents)
    --let voc = L.map W.readDouble (modWords $ contents)
    --L.putStrLn vec
    --let hnumb = head vec
    --L.putStrLn $ head vec
    --L.putStr $ head $ map (*2) vec
    let nvec = map normalize vec
    let dvec = map fromIntegral nvec
    let modvec = map (/100.0) dvec
    let ret = computeReturns modvec
    let crmat = corrMatrix 1 ([ret]++[ret]++[ret]++[ret])
    let crmat2 = fromLists crmat :: Matrix Double
    disp crmat2
    let eigvals =  (eigR crmat2)
    --print crmat
    L.putStrLn contents
    --print $ covariance ret ret
    --print vec
    --print nvec
    --print dvec
    --print modvec
    --L.putStrLn voc
    
--modWords :: L.ByteString -> [L.ByteString]
--modWords = T.words 
--modWords = Data.List.filter (not . L.null) . L.splitWith Data.ByteString.Internal.isSpaceWord8

disp = putStr . dispf 2

string2double:: String -> Double
string2double num = read num::Double

computeReturns:: [Double] -> [Double]
computeReturns (x:[]) = [10.0]
computeReturns [] = [0.0]
computeReturns prices = map log (zipWith (/) (tail prices) prices) 
--computeReturns (x:xs) = (head(xs)/x):(computeReturns xs) 


covariance:: [Double] -> [Double] -> Double
covariance x y = let meanx = (sum x) / fromIntegral (length x)
	             meany = (sum y) / fromIntegral (length y)
	             norx = map (+ (-meanx)) x
	             nory = map (+ (-meany)) y
		 in   (sum $ zipWith (*) norx nory) / fromIntegral (length x -1)

corrMatrix:: Int -> [[Double]] -> [[Double]]
--corrMatrix (pr:[]) = covariance pr pr
corrMatrix _ [] = []
corrMatrix n (pr:prs) = ((take n (repeat 1.0))++(map (\li -> (covariance pr li) / ((sqrt $ covariance li li)*(sqrt $ covariance pr pr)) ) prs)):(corrMatrix (n+1) prs) 

readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case C.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case C.readInt (C.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)

readPriceMod :: L.ByteString -> Maybe Int
readPriceMod str =
    case C.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case C.null rest of
	  True -> Just (dollars*100)
	  False -> case C.readInt (C.tail rest) of
                    Nothing           -> Just (dollars*100)
                    Just (cents,more) -> Just (dollars * 100 + cents)
                    
normalize:: Maybe Int -> Int
normalize (Just numb) = numb
normalize Nothing = 10
