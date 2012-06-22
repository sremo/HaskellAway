import Control.Applicative
import Control.Monad.Instances ()


newtype ZipListo a = ZipListo { getZipListo :: [a] } deriving (Show)

--instance Show (ZipListo a) where
--  show (ZipListo [a]) = show [a]


instance Functor ZipListo where
  fmap _ (ZipListo []) = ZipListo []
  fmap f (ZipListo xs) = ZipListo (map f xs) 

instance Applicative ZipListo where
  pure x = ZipListo [x]   
  (ZipListo gs) <*> (ZipListo xs) = ZipListo (zipWith ($) gs xs)

-- fmap f x = pure f <*> x
testZL:: Int -> (ZipListo (Int ->Int))
testZL a = pure (*a)

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)

-- instance Show (MyEither [Char] b) where
--     show (MyLeft s) = show s
--     show (MyRight _) = show "right value"

instance Functor (MyEither a) where
  fmap f (MyLeft a) = MyLeft a
  fmap f (MyRight b) = MyRight (f b)


newtype Arr e b = Arr {getFunction:: ((->) e b)}
--instance Functor ((->) e) where
--  fmap f (g) = \x -> f $ g x-- g =  e -> 

--instance Functor (Arr e) where
--    fmap f (g) = Arr(\x -> f $ g x)

-- fmap:: (a->b) -> f a -> f b

data Pair a = Pair a a deriving (Show)

instance Functor (Pair) where
  fmap f (Pair x y) = Pair (f x) (f y) 


data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance Functor (MyMaybe) where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

instance Applicative (MyMaybe) where
   pure = MyJust
   (<*>) MyNothing _ = MyNothing
   (<*>) _ MyNothing = MyNothing
   (<*>) (MyJust f) (MyJust x) = MyJust (f x)


(>>=) x f = join $ fmap f x

join x = x >>= (\t -> t)
fmap f x = x >>= (\t -> return $ f t)

mySequence:: (Monad m) => [m a] -> m [a]
mySequence xs = foldr (>>=) (return []) xs