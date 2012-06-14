-- Analyze frequencies of words in texts
-- Analyze frequencies of single words, average lengths of words and sentences and variability of words (letters in words)
import qualified Data.Set as Set  
import qualified Data.List as List

splitTokenize = map words . lines

text


let wordsList = List.nub $ List.concat $ List.map words . lines $ text
-- List.group $ List.sort $ List.concat $ List.map words $ lines text
-- CREATE SIMILARITY SORT

let freqWords 