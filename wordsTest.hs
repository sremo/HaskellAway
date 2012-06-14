import Control.Monad 
import Data.Char
import Data.List


custWords :: String -> [String]
--custWords :: [Char] -> [[Char]]
custWords " " = []-- ["Nothinghere!"]
custWords [] = [] -- ["EmptyList!"]
custWords (' ':xs) = custWords xs
--custWords sentence =  [firstW] ++ custWords restSentence
--  where (firstW, restSentence) = span (/=' ') sentence
custWords sentence
  | restSentence == [] = [filter (`elem` (['a'..'z']++['A'..'Z']++['0','1'..'9']))firstW]
  | otherwise = [filter (`elem` (['a'..'z']++['A'..'Z']++['0','1'..'9']))firstW] ++ custWords restSentence
    where (firstW, restSentence) = span (/=' ') sentence
	 
splitTokenize text = (map words . lines) text

main = forever $ do
    l <- getLine
    -- HOW TO PRINT ints???
    putStrLn $ "Length: " ++ (show $ length $ custWords l)
    putStrLn $ intercalate "-" $ custWords l
    --putStrLn toUpper l
    --putStrLn l
    --custWords l 