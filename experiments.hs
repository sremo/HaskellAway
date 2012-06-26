
group3 [] = [[]]
group3 xs  
       | length xs > 3 = [take 3 xs] ++ (group3 (drop 3 xs))
       | otherwise = [xs]


group3fold xs = foldl groupIt [[]] xs
              where groupIt acc el 
                            | (length $ last acc) == 3 = acc ++ [el]
                            | otherwise = init acc ++ [last acc ++ el]