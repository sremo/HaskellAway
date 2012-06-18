
myLookup:: (Eq a) => a -> [(a,b)] -> Maybe b
myLookup key ((ks,value):rest) = if (key == ks) then (Just value) else (myLookup key rest)
myLookup key [] = Nothing

myLookupList:: (Eq a) => a -> [(a,b)] -> [b]
myLookupList key lst = map snd $ filter (\(a,b) -> a == key) lst