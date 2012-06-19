
myFoldr:: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc (l:lst) = f l (myFoldr f acc lst)  
myFoldr _ acc [] = acc

myFoldl:: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc (l:lst) = myFoldl f (f acc l) lst
myFoldl _ acc [] = acc

myLookup:: (Eq a) => a -> [(a,b)] -> Maybe b
myLookup key ((ks,value):rest) = if (key == ks) then (Just value) else (myLookup key rest)
myLookup key [] = Nothing

myLookupList:: (Eq a) => a -> [(a,b)] -> [b]
myLookupList key lst = map snd $ filter (\(a,b) -> a == key) lst