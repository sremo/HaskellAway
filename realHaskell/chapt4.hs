
import Data.Char (digitToInt)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (x:xs) 
	| f x =  x:(myFilter f xs)
	| otherwise = myFilter f xs
myFilter _ _ = []


mySum xs = helperF 0 xs
    where helperF acc (x:xs) = helperF (acc+x) xs
          helperF acc _ = acc


foldFilter :: (a-> Bool) -> [a] ->[a]
foldFilter f xs = foldr step [] xs
	where step x xs | f x = x:xs
			| otherwise = xs


loopAsInt ::  Int -> String -> Int
loopAsInt acc [] = acc
loopAsInt acc (x:xs) = loopAsInt acc' xs
	where acc' = acc*10 + digitToInt x


asInt :: String -> Int
asInt xs = loopAsInt 0 xs



asIntFoldL :: String -> Int
asIntFoldL "-" = 0
asIntFoldL " " = 0
asIntFoldL ('-':xs) = -foldl f 0 xs
	where f a y = a*10 + digitToInt y
asIntFoldL xs = foldl f 0 xs
	where f a y = a*10 + digitToInt y

type ErrorMessage = String
asIntEither :: String -> Either ErrorMessage Int
asIntEither "-" = Left "Invalid String"
asIntEither " " = Left "Empty String"
--asIntEither ('-':xs) = (Right (-1))*foldl f (Right 0)
--	where f _ '.' = Left "Invalid character"
--	      f (Right a) y = Right(a*10 + digitToInt y)
--	      f (Left msg) y = Left "Invalid"

asIntEither xs = foldl f (Right 0) xs
	where f _ '.' = Left "Invalid character"
	      f (Right a) y = Right(a*10 +digitToInt y)
	      f (Left msg) y = Left "Invalid"
--asIntEither xs = foldr f (Right 0) xs

concat :: [[a]] -> [a]
concat all@((x:xs):((y:ys):zs)) = foldr (++) [] all


takeWhileRemo :: (a->Bool)->[a]->[a]
takeWhileRemo f [] = []
takeWhileRemo f (x:[]) 
	| f x = [x]
	| otherwise = []
takeWhileRemo f (x:xs)  
	| f x = x:(takeWhileRemo f xs) 
	| otherwise = []



groupByFold :: (a-> a-> Bool) -> [a]->[[a]]
groupByFold f (x:xs) = foldl g [[]] xs
	where g [[]] l = [l]
	      g (y:ys) l 
		| f (head y) l = ([l]++y):ys
		| otherwise = y:(g ys l)
--		| f x z = [x:(z:zs)]++[ys]
--		| otherwise = [z:zs]++(g ys x) 


splitLines [] = []
splitLines ln =
	let (pre, post) = break isLineBreaker ln
	in pre : case post of 
		('\r':'\n':rest) -> splitLines rest
		('\r':rest) -> splitLines rest
		('\n':rest) -> splitLines rest
		_ -> [] 

isLineBreaker char = char == '\n' || char == '\r'

fixLines :: String -> String
fixLines str = unlines $ splitLines str


isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] [] = True
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
	| length xs > length ys = False
	| x==y = isPrefixOf' xs ys 
	| x/=y = False


any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' cnd (x:xs) 
	| cnd x = True
	| otherwise = any' cnd xs

all' :: (a->Bool) -> [a] -> Bool
all' _ [] = True
all' cnd (x:xs) 
	| cnd x = all' cnd xs
	| otherwise = False


take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs) 
	| n > 0  = x:(take' (n-1) xs)
	| otherwise = []

drop' n [] = []
drop' n (x:xs)
	| n>0 = drop' (n-1) xs
	| otherwise = (x:xs)


splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs 
	| n>0 = (take' n xs, drop' n xs)
	| otherwise = ([], xs)

splitAt'' :: Int -> [a] -> ([a],[a])
splitAt'' n (x:xs) 
	| n>0 = let (pre, suf) = splitAt'' (n-1) xs in (x:pre, suf) 
	| otherwise = ([],(x:xs))


isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' [] _ = True
isInfixOf' _ [] = False
isInfixOf' xs (y:ys)  
	| isPrefixOf' xs (y:ys) = True
	| otherwise = isInfixOf' xs ys


zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

zipWith' :: (a ->b ->c) -> [a] ->[b]->[c]
zipWith' f [] [] = []
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith f xs ys)



safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs) 
	| null xs = Just x
	| otherwise = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:xs) 
	| null xs = Just []
	| otherwise = let safeInitM [] = []
	                  safeInitM (x:xs)
				| null xs = []
				| otherwise = x:safeInitM xs
		      in Just (x:(safeInitM xs))


safeListFunc f [] = Nothing
safeListFunc f xs = Just (f xs)


safeHead' = safeListFunc head
safeTail' = safeListFunc tail
safeLast' = safeListFunc last
safeInit' = safeListFunc init
