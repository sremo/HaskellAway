--custWords :: [Char] -> [[Char]]
custWords :: String -> [String] 
custWords " " = []--["Nothinghere!"]
custWords [] = []--["EmptyList!"]
custWords (' ':xs) = custWords xs
custWords sentence =  [firstW] ++ custWords restSentence
  where (firstW, restSentence) = span (/=' ') sentence
