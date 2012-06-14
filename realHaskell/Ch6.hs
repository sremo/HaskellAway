data Color = Blue | Green | Red
			deriving (Show, Read)
colorEq :: Color -> Color -> Bool
colorEq Blue Blue = True
colorEq Green Green = True
colorEq Red Red = True
colorEq _ _ = False


class BasicEq a where 
	isEqual :: a -> a -> Bool
	isEqual x y = not (isNotEqual x y)
	isNotEqual :: a -> a -> Bool
	isNotEqual x y = not (isEqual x y)
	--(==) :: BasicEq a => a -> a -> Bool
	--(/=) :: BasicEq a => a -> a -> Bool
	--x Main.== y = not(x Main./= y)
	--x Main./= y = not(x Main.== y)
	
instance BasicEq Bool where
	isEqual True True = True
	isEqual False False = True
	isEqual _ _ = False
	--True == True = True
	--False == False = True
	--_ == _ = False
	
instance BasicEq String where
	isEqual [] [] = True
	isEqual (x:xs) (y:ys) = (x Prelude.== y) && isEqual xs ys
	isEqual _ _ = False
	