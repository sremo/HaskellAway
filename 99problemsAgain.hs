myLast:: [a] -> Either String a
myLast [] = Left "Empty List!"
myLast (x:[]) = Right x
myLast (x:xs) = myLast xs

safeHead :: Either String [a] -> Either String a
safeHead (Left a) = Left a
safeHead (Right []) = Left "Empty List!"
safeHead (Right (x:xs)) = Right x


safeTail :: Either String [a] -> Either String [a]
safeTail (Left a) = Left a
safeTail (Right []) = Left "Empty List"
safeTail (Right xs) = Right $ tail xs

lastButOne :: [a] -> Either String a
lastButOne [] = Left "Empty List"
lastButOne (x:(y:[])) = Right x
lastButOne (x:[]) = Right x
lastButOne (x:xs) = lastButOne xs

eitherReverse a = Right a

myLast2 = safeHead.eitherReverse
lastButOne2 = safeHead.safeTail.eitherReverse


elementAt:: [a]-> Integer -> Either String a
elementAt [] _ = Left "List too short"
elementAt (x:xs) a
	| a <= 0  = Left "Negative index"
	| a == 1 = Right x
	| otherwise = elementAt xs (a-1)


myLength = foldr (\a b -> b+1) 0
myLength2 = foldl (\a b -> a+1) 0
myLength3:: [a] -> Integer
myLength3 (x:xs) = 1+myLength3 xs
myLength4 xs = fst $ last $ ( zip [1..] xs)


-- Problem 5: reverse a list

myReverse:: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs)++[x]

myReverse2:: [a] -> [a]
myReverse2 [] = []
myReverse2 (x:xs) = foldr (\a b -> b++[a]) [] (x:xs)

myReverse3:: [a] -> [a]
--yReverse3 [] = []
myReverse3 = foldl (\a b -> [b]++a) []

myReverse4:: [a] -> [a]
myReverse4 [] = []
myReverse4 xs = (last xs):(myReverse4 $ init xs)

-- Problem 6: check if list is a palindrome

isPalindrome:: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs =  xs == (myReverse xs)

isPalindrome2:: Eq a => [a] -> Bool
isPalindrome2 [] = True
isPalindrome2 (x:[]) = True
isPalindrome2 (x:xs) = x == ( last xs) && isPalindrome2 (init xs)


-- Problem 7: flatten a nested list structure

data Nested a  = Elem a | List [Nested a] deriving (Show)


flattenNL:: (Nested a) -> [a]
flattenNL (Elem x) = [x]
flattenNL (List []) = []
flattenNL (List (x:xs)) = (flattenNL x) ++ (flattenNL (List xs))

flattenNL2:: (Nested a) -> [a]
flattenNL2 (Elem x) = [x]
flattenNL2 (List all@(x:xs)) = concatMap flattenNL2 all


flattenNL3:: (Nested a) -> [a]
flattenNL3 (Elem x) = [x]
flattenNL3 (List xs) = foldr (++) [] $ map flattenNL3 xs
--flattenNL3 (List []) = []

-- Problem 8: eliminate consecutive duplicates in list

compress:: (Eq a) => [a] -> [a]
compress (x:[]) = [x]
compress [] = []
compress (x:(y:ys)) 
	| x == y = compress (y:ys)
	| otherwise = x:(compress (y:ys))

compress2:: (Eq a) => [a] -> [a]
compress2 (x:xs) = foldl (\a b -> if (last a == b) then a else a++[b]) [x] xs

compress3:: (Eq a) => [a] -> [a]
compress3 xs = foldr (\a b -> if (b==[]) then [a] else (if (head b == a) then b else (a:b))) [] xs


-- Problem 9: pack consecutive duplicates into sublists

pack:: (Eq a) => [a] -> [[a]]
pack (x:xs) =  foldl (\a b -> if ((last $ last a) == b) then (init a) ++ [(last a) ++ [b]] else a++[[b]])  [[x]] xs 

pack2:: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:[]) = [[x]]
pack2 (x:xs) = if elem x (head $ pack2 xs)
	       then [[x]++(head $ pack2 xs)]++(tail $ pack2 xs)
	       else [[x]]++(pack2 xs)

-- Problem 10: run length encoding of list
runLengthEnc:: (Eq a) => [a] -> [(a,Int)]
runLengthEnc = (map (\a -> (head a, length a))).pack

runLengthEnc2:: (Eq a) => [a] -> [(a,Int)]
runLengthEnc2 [] = []
runLengthEnc2 (x:xs) = (x,length $ takeWhile (==x) (x:xs)):(runLengthEnc2 $ dropWhile (==x) xs)


-- Problem 11: run length encoding modified

data SinMult a = Multiple Int a | Single a deriving (Show)

transSinMult:: (a,Int) -> SinMult a
transSinMult (a,nr)
	| nr == 1 = Single a
	| otherwise = Multiple nr a

sinMult2List:: SinMult a -> [a]
sinMult2List (Single x) = [x]
sinMult2List (Multiple nr x) = take nr (repeat x)

runLengthEncMod:: (Eq a) => [a] -> [SinMult a]
runLengthEncMod = (map transSinMult).runLengthEnc


-- Problem 12: decode run length encoding

decodeRLE:: [SinMult a] -> [a]
decodeRLE = concat.(map sinMult2List)

decodeRLE2 = concatMap sinMult2List

-- Problem 13: direct RLE
runLengthEncDir:: (Eq a) => [a] -> [SinMult a]
runLengthEncDir [] = []
runLengthEncDir (x:xs) = (transSinMult (x, (length $ takeWhile (==x) (x:xs) ))):(runLengthEncDir (dropWhile (==x) xs) )


-- Problem 14: duplicate elements of a list

duplicateEl:: [a] -> [a]
duplicateEl [] = []
duplicateEl (x:xs) = (x:(x:(duplicateEl xs)))

duplicateEl2:: [a] -> [a]
duplicateEl2 [] = []
duplicateEl2 (x:xs) = foldl (\a b -> a++[b,b]) [x,x] xs

duplicateEl3 = concatMap (\x -> [x,x])
duplicateEl4 = concatMap (replicate 2)


duplicateEl5 xs = xs >>= (\x -> [x,x])


-- Problem 15: replicate elements of a list n times

duplicateElXTimes:: Int -> [a] -> [a]
duplicateElXTimes n xs = xs >>= (\x -> take n (repeat x))


-- Problem 16: drop every nth element


dropEveryNth:: Int -> [a] -> [a]

dropEveryNth n xs
	| n >= length xs = xs
	| otherwise = (init fIRST) ++ (dropEveryNth n sECOND)
		where (fIRST, sECOND) = splitAt n xs


-- Problem 17: split at

--mySplitAt xs n 
--	| n <= 0 = ([],xs)
--	| n >= length xs = (xs,[])
--	| otherwise = helpFunPair n xs
--		where helpFunPair 0 ys = ([],ys)
--		      helpFunPair 1 (x:ys) = (x,ys)
--		      helpFunPair n (x:ys) = ([x]++[(fst $ helpFunPair (n-1) ys)],snd $ helpFunPair (n-1) ys) 

mySplitHlp (x:xs) n 
	| n <= 0 = [[],(x:xs)]
	| n >= length (x:xs) = [(x:xs),[]]
	| otherwise = [x:(head (mySplitHlp xs (n-1))),last $ mySplitHlp xs (n-1)]

mySplit (x:xs) n = (head $ mySplitHlp (x:xs) n, last $ mySplitHlp (x:xs) n)


firstN [] n = []
firstN (x:xs) n 
	| n <= 0 = []
	| n >= length (x:xs) = (x:xs)
	| otherwise = x:(firstN xs (n-1))

afterN [] n = []
afterN (x:xs) n
	| n <= 0 = (x:xs)
	| n >= length (x:xs) = []
	| otherwise = afterN xs (n-1)



mySlice:: [a] -> Int -> Int -> [a]
mySlice xs a b 
	| a > b = []
	| a <= 0 = firstN xs b
	| b <= 0 = []
	| b > length xs = afterN xs (a-1)
	| otherwise = firstN (afterN xs (a-1)) (b-a+1)

myRotate:: [a] -> Int -> [a]
myRotate xs a
	| a > 0 = (afterN xs a)++(firstN xs a)
	| a < 0 = (afterN xs (length xs+a))++(firstN xs (length xs + a))
	| a == 0 = xs

myRemoveAt:: [a] -> Int -> ([a], [a])
myRemoveAt xs nr = (mySlice xs nr nr, (firstN xs (nr-1))++(afterN xs nr))


myInsertAt:: [a] -> Int -> a -> [a]
myInsertAt xs nr x = (firstN xs (nr-1)) ++ [x] ++ (afterN xs (nr-1))

myRange:: Int -> Int -> [Int]
myRange a b
	| a > b = []
	| a == b = [a]
	| a < b = a:myRange (a+1) b

