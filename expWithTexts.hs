

-- playing around with haskell and natural language processing

import qualified Data.Text as L
import qualified Data.List as DLST
import Data.Text.IO

freqList:: L.Text -> [(Int,L.Text)]
freqList xs = DLST.sortBy (\(xa,xb) (ya,yb) -> if xa == ya then (if xb>=yb then GT else LT) else (if xa > ya then LT else GT )) (encRLE $ DLST.sort $ L.words xs)


encRLE:: (Eq a) => [a] -> [(Int, a)]
encRLE = encode 1
	where encode _ [] = []
	      encode n (x:[]) = [(n,x)]
	      encode n (x:xs) = if (head xs) == x then encode (n+1) xs else (n,x):(encode 1 xs) 

myWordsFreqList:: FilePath -> IO [(Int,L.Text)]
myWordsFreqList path = do
	content <- Data.Text.IO.readFile path
	return (freqList content)
