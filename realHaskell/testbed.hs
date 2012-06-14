-- File testbed.hs
-- Trying some simple Haskell code

addtwo :: Num a => a -> a -> a
--addtwo :: Int -> Int -> Int
addtwo a b = a + b

length' :: Num a => [t] -> a
--length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1+length' xs

mean' :: Fractional a => [a] -> Maybe a
mean' [] = Nothing
mean' all@(x:xs) = Just ((sum all) / (fromIntegral (length all)))

unwrappa :: Num a => Maybe a -> a
unwrappa ao = case ao of
                 Just ao -> ao
                 Nothing -> 0

rever :: [a] -> [a]
rever [] =  []
rever (x:xs) = (rever xs)++ (x:[])

palindromator :: [a] -> [a]
palindromator [] = []
palindromator a = a ++ rev a 
                where rev (x:xs) = (rev xs) ++ (x:[])
                      rev [] = []   

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = if x == y 
                         then isPalindrome ys
                      else False
                      where (y:ys) = rever xs 

sortListList ::  [[a]] -> [[a]]
sortListList [] = []
sortListList (x:[]) = [x]
sortListList all@(x:(y:[])) = if length x > length y
                             then (y:(x:[]))
                          else all
sortListList (x:xs) = (sortListList a) ++[x] ++c  ++ (sortListList b)
                      where  l = length x
                             a = [m | m<-xs, (length m) < l ]
                             b = [m | m<-xs, (length m) > l ]
                             c = [m | m<-xs, (length m) == l]


intersperse :: a -> [[a]] -> [a]
intersperse _ [[]] = []
intersperse i (x:[]) = x
intersperse i (x:xs) = x++[i] ++(intersperse i xs)

newLast :: [a] -> a
newLast [] = error "There is no last element in an empty list" 
newLast (x:[]) = x 
newLast (x:xs) = newLast xs

lastButOne :: [a] -> a
lastButOne [] = error "No elements in an empty list"
lastButOne (x:[]) = error "This list is too short."
lastButOne (x:(y:[])) = x
lastButOne (x:xs) = lastButOne xs


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)
data EnhcdTree a = EnhcdNode a (Maybe (EnhcdTree a)) (Maybe (EnhcdTree a))
                   deriving (Show)

convFromList :: List a -> [a]
convFromList Nil = []
convFromList (Cons x xs) = (x:(convFromList xs))

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node a b Empty) = 1 + treeDepth b
treeDepth (Node a Empty b) = 1 + treeDepth b
treeDepth (Node a b c) = 1 + max (treeDepth b) (treeDepth c)

data Direction = PosOr | NegOr | Straight
                  deriving (Show)
type Xpos = Int
type Ypos = Int
data Position = Position Xpos Ypos
                deriving (Show)

dirFunc :: Position -> Position -> Position -> Direction
--dirFunc _ _ = error "Not enough inputs"
dirFunc (Position ax ay) (Position bx by) (Position cx cy) 
       | val == 0 = Straight
       | val < 0 = NegOr
       | val > 0 = PosOr
      where val =  ((bx-ax)*(cy-ay) -(by-ay)*(cx-ax))
                      
dirFuncList :: [Position] -> [Direction]
dirFuncList [] = error "Empty list"
dirFuncList (x:[]) = error "Your list is too short"
dirFuncList (x:(y:[])) = error "Your list is too short"
dirFuncList (x:(y:(z:zs))) = (dirFunc x y z):dirFuncList (y:(z:zs)) 

--type movieID = Int
--type movieName = String
--type directorName = String

--data movieInfo = movie movieID movieName directorName
--                 deriving (Show)
