
myLast:: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myLast2:: [a] -> a
myLast2 (x:xs) = case xs of
		[] -> x
		_ -> myLast2 xs


myButLast:: [a] -> a
myButLast (x:[]) =x
myButLast xs = head $ tail $ reverse xs

myButLast2:: [a] -> a
myButLast2 (x:xs) = case xs of
			[] -> x
			(y:[]) -> x
			_ -> myButLast2 xs


elementAt:: [a]-> Int -> Maybe a
elementAt xs k | k<= 0 = Nothing
	       | (length xs) < k = Nothing
	       | k == 1 = Just $ head xs
	       | True  = elementAt (tail xs) (k-1)


myLength:: Num a => [a] -> a
myLength x = foldl (\a b -> a+1) 0 x 
	

myReverse:: [a] -> [a]
myReverse (x:[]) = [x]
myReverse [] = []
myReverse xs = [last xs] ++ (myReverse $ init xs)

myReverse2:: [a] -> [a]
myReverse2 (x:[]) = [x]
myReverse2 xs = foldr (\a b -> b++[a]) [] xs


isPalindrome::Eq a=> [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) | x == (last xs) = isPalindrome $ init xs
		    | otherwise = False


elimDupl:: Eq a => [a] -> [a]
elimDupl [] = []
elimDupl (x:[]) = [x]
elimDupl (x:y:xs) 
		| x == y = (elimDupl (y:xs))
		| otherwise = [x]++elimDupl (y:xs)

groupDupl:: Eq a => [a] -> [[a]]
groupDupl [] = []
groupDupl (x:[]) = [[x]]
groupDupl (x:y:xs)
		| x == y = isNextIn [x,y] x xs
		| otherwise = [[x]] ++ (isNextIn [y] y xs)
		where isNextIn partL curr (l:ls) 
						| curr == l = isNextIn (partL++[l]) l ls
						| otherwise = [partL] ++ (isNextIn [l] l ls)
		      isNextIn partL curr [] = [partL]

myEncode:: Eq a => Show a => [a] -> [(Int, a)]
myEncode (xs) = map countPair $ groupDupl xs
		where countPair all@(x:xs) = (length all, x)

data EncElem = Single String | Multiple Int String
		deriving (Show)
myEncode2:: (Eq a, Show a) => [a] -> [EncElem]
myEncode2 xs = map countPair $ groupDupl xs
		where countPair all@(x:ys) 
					| length all == 1 = Single (show x)
					| otherwise = Multiple (length all) (show x)
